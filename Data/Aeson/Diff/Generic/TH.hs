{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf,
ExistentialQuantification, TemplateHaskell #-}
module Data.Aeson.Diff.Generic.TH
  where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Data.Aeson
import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.List
import Data.Aeson.Diff.Generic.Types
import Text.Read
import qualified Data.Text as T
import Control.Monad
import Data.Maybe

data GetSetPure s = forall v. JsonPatch v => GetSetPure v (v -> s)

type PathLens s = s -> Path -> Result (Path, Path, GetSetPure s)

getAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s
                     -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointerFromLens _ (Pointer []) s f = pure $ f s
getAtPointerFromLens l (Pointer path) s f = do
    (_, subPath, GetSetPure subS _) <- l s path
    getAtPointer (Pointer subPath) subS f

movePathFromLens :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s
                 -> Result s
movePathFromLens _ (Pointer []) (Pointer []) s = pure s
movePathFromLens _ (Pointer []) (Pointer _) _ =
  Error "Cannot move to subpath"
movePathFromLens l (Pointer fromPath) (Pointer toPath) s = do
  (pref, toSubPath, GetSetPure x setr) <- l s toPath
  if pref `isPrefixOf` toPath
    then setr <$> movePath (Pointer (take (length pref) fromPath))
         (Pointer toSubPath) x
    else do (v, s') <- deleteAtPointer (Pointer fromPath) s toDyn
            addAtPointer (Pointer toPath) s' v getDynamic

copyPathFromLens :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s
                 -> Result s
copyPathFromLens _ (Pointer []) (Pointer []) s = pure s
copyPathFromLens l (Pointer fromPath) (Pointer toPath) s = do
  v <- getAtPointerFromLens l (Pointer fromPath) s toDyn
  addAtPointer (Pointer toPath) s v getDynamic

replaceAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
                            (forall v.JsonPatch v => r -> Result v) -> Result s
replaceAtPointerFromLens _ (Pointer []) _ v f = f v
replaceAtPointerFromLens l (Pointer path) s val f = do
  (_, subPath, GetSetPure x setr) <- l s path
  setr <$> replaceAtPointer (Pointer subPath) x val f

testAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPointerFromLens _ (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPointerFromLens l (Pointer path) s val f = do
  (_, subPath, GetSetPure x _) <- l s path
  _ <- testAtPointer (Pointer subPath) x val f
  pure s

keyPat :: Key -> PatQ
keyPat (OKey str) = conP 'OKey [litP $ stringL $ T.unpack str]
keyPat (AKey i) = [p| AKey $(litP $ integerL $ fromIntegral i) |]

keyExp :: Key -> ExpQ
keyExp (OKey str) =  [| OKey $(litE $ stringL $ T.unpack str) |]
keyExp (AKey i) = [| AKey i |]
                
makeKey :: String -> Key
makeKey str = case readMaybe str of
  Nothing -> OKey $ T.pack str
  Just i -> AKey i

appendP :: [PatQ] -> PatQ -> PatQ
appendP k end = foldr consP end k where
  consP :: PatQ -> PatQ -> PatQ
  consP x y = conP '(:) [x, y]
  
select :: [a] -> [([a], a, [a])]
select [] = []
select l@(_:lt) = zip3 (inits l) l (tails lt)

appListE :: ExpQ -> [ExpQ] -> ExpQ
appListE = foldl appE

invalidMatch :: MatchQ
invalidMatch = match wildP (normalB [| Error "Invalid path" |]) []

-- create matches for positional fields
makePosCases :: Name -> [Key] -> Name -> Int -> MatchQ
makePosCases pathVar prefix consName nFields = do
  vs <- replicateM nFields $ newName "var"
  v2 <- newName "var"
  subPath <- newName "path"
  let mkPosMatch :: ([Name], Name, [Name]) -> Integer -> MatchQ
      mkPosMatch (p, v, n) i =
        match ([p| (AKey $(litP $ integerL i): $(varP subPath)) |])
        (normalB [| pure [ $(listE $ (keyExp <$> prefix) ++ [
                                appE (conE 'AKey) (litE $ IntegerL i)])
                         , $(varE subPath)
                         , GetSetPure $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ] |])
        []
  match (conP consName $ map varP vs) 
    (normalB $ caseE (varE pathVar) $
     zipWith mkPosMatch (select vs) [0..] ++ [invalidMatch])
    []

-- create matches for record fields
makeRecCases :: Name -> [Key] -> [String] -> Name -> MatchQ
makeRecCases pathVar prefix recordFields consName = do
  vs <- mapM (const $ newName "var") recordFields
  v2 <- newName "var"
  subPath <- newName "path"
  let mkRecMatch :: ([Name], Name, [Name]) -> String -> MatchQ
      mkRecMatch (p, v, n) fieldName =
        match ([p| $(keyPat $ makeKey fieldName): $(varP subPath) |])
        (normalB [| pure [ $(listE $ (keyExp <$> prefix) ++
                              [keyExp $ makeKey fieldName])
                         , $(varE subPath)
                         , GetSetPure $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ] |])
        []
  match (conP consName $ map varP vs)
    (normalB $ caseE (varE pathVar) $
     zipWith mkRecMatch (select vs) recordFields ++ [invalidMatch])
    []
    
-- create a match for a single positional field
makeSingleCase :: Name -> [Key] -> Name -> MatchQ
makeSingleCase pathVar prefix consName = do
  v <- newName "var"
  v2 <- newName "var"
  subPath <- newName "path"
  match (conP consName [varP v])
    (normalB $ caseE (varE pathVar)
     [ match (appendP (map keyPat prefix) (varP subPath))
       (normalB [| pure ( $(listE $ map keyExp prefix)
                        , $(varE subPath)
                        , GetSetPure $(varE v)
                          $(lamE [varP v2] $
                            appE (conE consName) $
                            varE v2)) |])
       []
     , invalidMatch])
    []

makePathLens :: Options -> Name -> Q (Name, Dec)
makePathLens options name = do
  typeInfo <- reifyDatatype name
  funName <- newName "pathLens"
  struc <- newName "struc"
  pathVar <- newName "path"
  let nConstructors = length $ datatypeCons typeInfo
      isTaggedObject = case sumEncoding options of
        TaggedObject _ _ -> True
        _ -> False

      makeCase :: ConstructorInfo -> Maybe MatchQ
      makeCase consInfo =
        let prefix
              | (nConstructors == 1) &&
                not (tagSingleConstructors options) = []
              | otherwise = case sumEncoding options of
                  UntaggedValue -> []
                  TaggedObject _ contentsName ->
                    [makeKey contentsName]
                  TwoElemArray -> [AKey 1]
                  ObjectWithSingleField ->
                    [makeKey $ constructorTagModifier options $
                     nameBase $ constructorName consInfo]
        in case constructorVariant consInfo of
          RecordConstructor [] -> Nothing
          RecordConstructor [_]
            | unwrapUnaryRecords options &&
              -- unwrapping is not done for taggedObject with multiple
              -- constructors
              (nConstructors == 1 || not isTaggedObject) ->
                Just $ makeSingleCase pathVar prefix $
                constructorName consInfo
          RecordConstructor fieldNames ->
            Just $ makeRecCases pathVar
            -- fields are unpacked into the object with TaggedObject
            (if isTaggedObject then [] else prefix)
            (map (fieldLabelModifier options . nameBase) fieldNames) $
            constructorName consInfo
          _ -> case length $ constructorFields consInfo of
                 0 -> Nothing
                 1 -> Just $ makeSingleCase pathVar prefix $
                      constructorName consInfo
                 n -> Just $ makePosCases pathVar prefix
                      (constructorName consInfo) n

      cases :: [MatchQ]
      cases = mapMaybe makeCase (datatypeCons typeInfo)

      lensBody = case cases of
        [] -> [| Error "Invalid Path" |]
        _ -> caseE (varE struc) (cases ++ [invalidMatch])
          
  lensDescr <- funD funName [
    clause [varP struc, varP pathVar] (normalB lensBody) []]
  pure (funName, lensDescr)
