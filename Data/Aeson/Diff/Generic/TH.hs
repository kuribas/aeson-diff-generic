{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf,
ExistentialQuantification, TemplateHaskell #-}
module Data.Aeson.Diff.Generic.TH
  where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.List
import Data.Aeson.Diff.Generic.Types
import Text.Read
import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import Control.Applicative

data GetSetPure s = forall v. JsonPatch v => GetSetPure Bool v (v -> s)
type PathLens s = s -> Path -> Result (Path, Path, GetSetPure s)

data GetSetMaybe s = forall v. JsonPatch (Maybe v) =>
                     GetSetMaybe (Maybe v) (Maybe v -> s)
-- a lens which only works on record fields which have type Maybe
type PathLensMaybe s = s -> Path -> Result (GetSetMaybe s)

getAtPointerTH :: (JsonPatch s) => PathLens s -> Pointer -> s
                     -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointerTH _ (Pointer []) s f = pure $ f s
getAtPointerTH l (Pointer path) s f = do
  (_, subPath, GetSetPure _ subS _) <- l s path
  getAtPointer (Pointer subPath) subS f

movePathTH :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s
                 -> Result s
movePathTH _ (Pointer []) (Pointer []) s = pure s
movePathTH _ (Pointer []) (Pointer _) _ =
  Error "Cannot move to subpath"
movePathTH l (Pointer fromPath) (Pointer toPath) s = do
  (pref, toSubPath, GetSetPure _ x setr) <- l s toPath
  if pref `isPrefixOf` toPath
    then setr <$> movePath (Pointer (take (length pref) fromPath))
         (Pointer toSubPath) x
    else do (v, s') <- deleteAtPointer (Pointer fromPath) s toDyn
            addAtPointer (Pointer toPath) s' v getDynamic

copyPathTH :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s
                 -> Result s
copyPathTH _ (Pointer []) (Pointer []) s = pure s
copyPathTH l (Pointer fromPath) (Pointer toPath) s = do
  v <- getAtPointerTH l (Pointer fromPath) s toDyn
  addAtPointer (Pointer toPath) s v getDynamic

replaceAtPointerTH :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
                            (forall v.JsonPatch v => r -> Result v) -> Result s
replaceAtPointerTH _ (Pointer []) _ v f = f v
replaceAtPointerTH l (Pointer path) s val f = do
  (_, subPath, GetSetPure _ x setr) <- l s path
  setr <$> replaceAtPointer (Pointer subPath) x val f

addAtPointerTH :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
                  (forall v.JsonPatch v => r -> Result v) -> Result s
addAtPointerTH _ (Pointer []) _ v f = f v                  
addAtPointerTH l (Pointer path) s val f = do
  (_, subPath, GetSetPure isRecord x setr) <- l s path
  if null subPath && not isRecord
    then Error "Cannot add value to non record field"
    else setr <$> addAtPointer (Pointer subPath) x val f

deleteAtPointerTH :: JsonPatch s => PathLens s -> PathLensMaybe s
                  -> Pointer -> s -> (forall v.JsonPatch v => v -> r)
                  -> Result (r, s)
deleteAtPointerTH _ _ (Pointer []) _ _ =
  Error "Invalid path"
deleteAtPointerTH l1 l2 (Pointer path) s f =
  (do GetSetMaybe v setr <- l2 s path
      if isNothing v
        then Error "fallthrough"
        else pure $ (f v, setr Nothing)) <|>
  (do (_, subPath, GetSetPure _ x setr) <- l1 s path
      fmap setr <$> deleteAtPointer (Pointer subPath) x f)
      
testAtPointerTH :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPointerTH _ (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPointerTH l (Pointer path) s val f = do
  (_, subPath, GetSetPure _ x _) <- l s path
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
        (normalB [| pure ( $(listE $ (keyExp <$> prefix) ++ [
                                appE (conE 'AKey) (litE $ IntegerL i)])
                         , $(varE subPath)
                         , GetSetPure False $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                            varE <$> (p ++ v2 : n))
                         ) |])
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
        (normalB [| pure ( $(listE $ (keyExp <$> prefix) ++
                              [keyExp $ makeKey fieldName])
                         , $(varE subPath)
                         , GetSetPure True $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ) |])
        []
  match (conP consName $ map varP vs)
    (normalB $ caseE (varE pathVar) $
     zipWith mkRecMatch (select vs) recordFields ++ [invalidMatch])
    []

isMaybe :: Type -> Bool
isMaybe (AppT (ConT name) _) = name == ''Maybe 
isMaybe _ = False

getMaybeVars :: [Type] -> [Name] -> [([Name], Name, [Name])]
getMaybeVars types vars =
  map snd $ filter (isMaybe . fst) $
  zip types $ select vars
 
-- create matches for record fields
makeMaybeRecCases :: Name -> [Key] -> [String] -> [Type] -> Name -> Maybe MatchQ
makeMaybeRecCases _ _ _ types _
  | not $ any isMaybe types = Nothing
makeMaybeRecCases pathVar prefix recordFields types consName = Just $ do
  vs <- mapM (const $ newName "var") recordFields
  v2 <- newName "var"
  let mkRecMatch :: ([Name], Name, [Name]) -> String -> MatchQ
      mkRecMatch (p, v, n) fieldName =
        match (listP $ [keyPat $ makeKey fieldName])
        (normalB [| pure ( GetSetMaybe $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ) |])
        []
  match (conP consName $ map varP vs)
    (normalB $ caseE (varE pathVar) $
     zipWith mkRecMatch (getMaybeVars types vs) recordFields ++ [invalidMatch])
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
                        , GetSetPure False $(varE v)
                          $(lamE [varP v2] $
                            appE (conE consName) $
                            varE v2)) |])
       []
     , invalidMatch])
    []

declarePathLens :: Options -> Name -> DecsQ
declarePathLens options name =
  (:[]) . snd <$> makePathLens options name

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

-- return a lens that only works on maybe fields.  Ugh, so much duplication...
makeMaybeLens :: Options -> Name -> Q (Name, Dec)
makeMaybeLens options name = do
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
                Nothing
          RecordConstructor fieldNames ->
            if omitNothingFields options then 
              makeMaybeRecCases pathVar
              -- fields are unpacked into the object with TaggedObject
              (if isTaggedObject then [] else prefix)
              (map (fieldLabelModifier options . nameBase) fieldNames)
              (constructorFields consInfo)
              (constructorName consInfo)
            else Nothing
          _ -> Nothing

      cases :: [MatchQ]
      cases = mapMaybe makeCase (datatypeCons typeInfo)

      lensBody = case cases of
        [] -> [| Error "Invalid Path" |]
        _ -> caseE (varE struc) (cases ++ [invalidMatch])
          
  lensDescr <- funD funName [
    clause [varP struc, varP pathVar] (normalB lensBody) []]
  pure (funName, lensDescr)
