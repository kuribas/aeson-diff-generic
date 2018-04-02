{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, ConstraintKinds,
    ExistentialQuantification, TemplateHaskell, PatternGuards #-}
{-|
This module contains functions to automatically derive `JsonPatch` instances.
-}


module Data.Aeson.Diff.Generic.TH (deriveJsonPatch)
  where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.List
import Data.Aeson.Diff.Generic.Types
import Text.Read.Compat
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

-- | Derive a JsonPatch instance, using the given aeson `Options`.
-- This should work with a `ToJSON` and `FromJSON` instance that uses the
-- same options.
deriveJsonPatch :: Options -> Name -> DecsQ
deriveJsonPatch options name = do
  (pathLensName, pathLensDecl) <- makePathLens options name
  (mbLensName, mbLensDecl) <- makeMaybeLens options name
  sigVars <- datatypeVars <$> reifyDatatype name
  vars <- mapM (const $ newName "a") sigVars
  let appliedType = foldl appT (conT name) $ map varT vars
      constrained =
        forallT (map PlainTV vars) $ 
        mapM (\v -> [t| JsonPatch $(varT v) |])
        vars
  pathLensSig <- sigD pathLensName $ constrained 
    [t| $(appliedType) -> Path ->
        Result (Path, Path, GetSetPure $(appliedType)) |]
  mbLensSig <- sigD mbLensName $ constrained
    [t| $(appliedType) -> Path -> Result (GetSetMaybe $(appliedType)) |]
  context <- mapM (\v -> [t| JsonPatch $(varT v) |]) vars
  classDecl <- instanceD (pure context)
               [t| JsonPatch $(appliedType) |]
    [ funD 'getAtPointer [
        clause [] (normalB [| getAtPointerTH $(varE pathLensName) |]) []]
    , funD 'deleteAtPointer [
        clause [] (normalB [| deleteAtPointerTH $(varE pathLensName)
                             $(varE mbLensName) |]) []]
    , funD 'addAtPointer [
        clause [] (normalB [| addAtPointerTH $(varE pathLensName) |]) []]
    , funD 'movePath [
        clause [] (normalB [| movePathTH $(varE pathLensName) |]) []]
    , funD 'copyPath [
        clause [] (normalB [| copyPathTH $(varE pathLensName) |]) []]
    , funD 'replaceAtPointer [
        clause [] (normalB [| replaceAtPointerTH $(varE pathLensName) |]) []]
    , funD 'testAtPointer [
        clause [] (normalB [| testAtPointerTH $(varE pathLensName) |]) []]
    ]
  pure [pathLensSig, pathLensDecl, mbLensSig, mbLensDecl, classDecl]


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
        else pure (f v, setr Nothing)) <|>
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

-- match against the key and the rest of the list
matchKey :: Key -> PatQ -> ExpQ -> MatchQ
matchKey (OKey str) rest e = do
  strVar <- newName "str"
  match (conP 'OKey [varP strVar] `consP` rest)
    (guardedB [liftA2 (,)
               (normalG [| $(varE strVar) ==
                          T.pack $(litE $ stringL $ T.unpack str)
                         |])
                e])
    []

matchKey (AKey i) rest e =
  match (conP 'AKey [litP $ integerL $ fromIntegral i] `consP` rest)
  (normalB e) []

keyExp :: Key -> ExpQ
keyExp (OKey str) =  [| OKey $ T.pack $(litE $ stringL $ T.unpack str) |]
keyExp (AKey i) = [| AKey i |]
                
makeKey :: String -> Key
makeKey str = case readMaybe str of
  Nothing -> OKey $ T.pack str
  Just i -> AKey i

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
makePosCases :: Name -> Maybe Key -> Name -> Int -> MatchQ
makePosCases pathVar prefix consName nFields = do
  vs <- replicateM nFields $ newName "var"
  v2 <- newName "var"
  subPath <- newName "path"
  let mkPosMatch :: ([Name], Name, [Name]) -> Integer -> MatchQ
      mkPosMatch (p, v, n) i =
        matchKey (AKey $ fromIntegral i) (varP subPath)
        [| pure ( $(listE $ maybeToList (keyExp <$> prefix) ++ [
                       appE (conE 'AKey) (litE $ IntegerL i)])
                , $(varE subPath)
                , GetSetPure False $(varE v)
                  $(lamE [varP v2] $
                    appListE (conE consName) $
                    varE <$> (p ++ v2 : n))
                )
         |]
      casePrefix f = case prefix of
        Nothing -> f pathVar 
        Just key -> do
          subPathVar <- newName "subPath"
          caseE (varE pathVar)
            [matchKey key (varP subPathVar) (f subPathVar), invalidMatch]

  match (conP consName $ map varP vs)
    (normalB $ casePrefix $ \subPathVar ->
        caseE (varE subPathVar) $
        zipWith mkPosMatch (select vs) [0..] ++ [invalidMatch] )
    []

-- create matches for record fields
makeRecCases :: Name -> Maybe Key -> [String] -> Name -> MatchQ
makeRecCases pathVar prefix recordFields consName = do
  vs <- mapM (const $ newName "var") recordFields
  v2 <- newName "var"
  subPath <- newName "path"
  let mkRecMatch :: ([Name], Name, [Name]) -> String -> MatchQ
      mkRecMatch (p, v, n) fieldName =
        matchKey (makeKey fieldName) (varP subPath)
        [| pure ( $(listE $ maybeToList (keyExp <$> prefix) ++
                    [keyExp $ makeKey fieldName])
                , $(varE subPath)
                , GetSetPure True $(varE v)
                  $(lamE [varP v2] $
                    appListE (conE consName) $
                    varE <$> (p ++ v2 : n))
                )
         |]
      casePrefix f = case prefix of
        Nothing -> f pathVar 
        Just key -> do
          subPathVar <- newName "subPath"
          caseE (varE pathVar)
            [matchKey key (varP subPathVar) (f subPathVar), invalidMatch]

  match (conP consName $ map varP vs)
    (normalB $ casePrefix $ \subPathVar ->
        caseE (varE subPathVar) $
        zipWith mkRecMatch (select vs) recordFields ++ [invalidMatch])
    []

isMaybe :: Type -> Bool
isMaybe (AppT (ConT name) _) = name == ''Maybe 
isMaybe _ = False

getMaybeVars :: [Type] -> [Name] -> [([Name], Name, [Name])]
getMaybeVars types vars =
  map snd $ filter (isMaybe . fst) $
  zip types $ select vars

makePathLensName :: String -> String -> String
makePathLensName prefix ('(':r)
  | (n, ")") <- span (== ',') r = 
      prefix ++ "Tuple" ++ show (length n)
makePathLensName prefix s = prefix ++ s
 
-- create matches for record fields
makeMaybeRecCases :: Name -> Maybe Key -> [String] -> [Type] -> Name -> Maybe MatchQ
makeMaybeRecCases _ _ _ types _
  | not $ any isMaybe types = Nothing
makeMaybeRecCases pathVar prefix recordFields types consName = Just $ do
  vs <- mapM (const $ newName "var") recordFields
  v2 <- newName "var"
  let mkRecMatch :: ([Name], Name, [Name]) -> String -> MatchQ
      mkRecMatch (p, v, n) fieldName =
        matchKey (makeKey fieldName) (listP [])
        [| pure ( GetSetMaybe $(varE v)
                  $(lamE [varP v2] $
                    appListE (conE consName) $
                    varE <$> (p ++ v2 : n))
                ) |]
      casePrefix f = case prefix of
        Nothing -> f pathVar 
        Just key -> do
          subPathVar <- newName "subPath"
          caseE (varE pathVar)
            [matchKey key (varP subPathVar) (f subPathVar), invalidMatch]

  match (conP consName $ map varP vs)
    (normalB $ casePrefix $ \subPathVar ->
        caseE (varE subPathVar) $
     zipWith mkRecMatch (getMaybeVars types vs) recordFields ++ [invalidMatch])
    []

-- create a match for a single positional field
makeSingleCase :: Name -> Maybe Key -> Name -> MatchQ
makeSingleCase pathVar prefix consName = do
  v <- newName "var"
  v2 <- newName "var"
  subPath <- newName "path"
  match (conP consName [varP v])
    (normalB $ case prefix of
         Nothing ->
           [| pure ( $(listE [])
                   , $(varE pathVar)
                   , GetSetPure False $(varE v)
                     $(lamE [varP v2] $
                       appE (conE consName) $
                       varE v2)) |]
         Just key ->
           caseE (varE pathVar) [
           matchKey key (varP subPath)
             [| pure ( $(listE [keyExp key])
                     , $(varE subPath)
                     , GetSetPure False $(varE v)
                       $(lamE [varP v2] $
                         appE (conE consName) $
                         varE v2)) |]
           , invalidMatch]
    ) []

makePathLens :: Options -> Name -> Q (Name, Dec)
makePathLens options name = do
  typeInfo <- reifyDatatype name
  let funName = mkName $ makePathLensName "generatedPathLensFor" $
                nameBase name
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
                not (tagSingleConstructors options) = Nothing
              | otherwise = case sumEncoding options of
                  UntaggedValue -> Nothing
                  TaggedObject _ contentsName ->
                    Just $ makeKey contentsName
                  TwoElemArray -> Just $ AKey 1
                  ObjectWithSingleField ->
                    Just $ makeKey $ constructorTagModifier options $
                    nameBase $ constructorName consInfo
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
            (if isTaggedObject then Nothing else prefix)
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

      lensClause = case cases of
        [] -> clause [wildP, wildP] (normalB [| Error "Invalid Path" |]) []
        _ -> clause [varP struc, varP pathVar] (
          normalB $
          caseE (varE struc) $
          cases ++ if length cases == nConstructors
            then [] else  [invalidMatch]
          ) []

  lensDecl <- funD funName [lensClause]
  pure (funName, lensDecl)

-- return a lens that only works on maybe fields.  Ugh, so much duplication...
makeMaybeLens :: Options -> Name -> Q (Name, Dec)
makeMaybeLens options name = do
  typeInfo <- reifyDatatype name
  let funName = mkName $ makePathLensName "generatedMaybePathLensFor" $
                nameBase name
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
                not (tagSingleConstructors options) = Nothing
              | otherwise = case sumEncoding options of
                  UntaggedValue -> Nothing
                  TaggedObject _ contentsName ->
                    Just $ makeKey contentsName
                  TwoElemArray -> Just $ AKey 1
                  ObjectWithSingleField ->
                    Just $ makeKey $ constructorTagModifier options $
                     nameBase $ constructorName consInfo
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
              (if isTaggedObject then Nothing else prefix)
              (map (fieldLabelModifier options . nameBase) fieldNames)
              (constructorFields consInfo)
              (constructorName consInfo)
            else Nothing
          _ -> Nothing

      cases :: [MatchQ]
      cases = mapMaybe makeCase (datatypeCons typeInfo)

      lensClause = case cases of
        [] -> clause [wildP, wildP] (normalB [| Error "Invalid Path" |]) []
        _ -> clause [varP struc, varP pathVar] (
          normalB $
          caseE (varE struc) $
          cases ++ if length cases == nConstructors
            then [] else  [invalidMatch]
          ) []
          
  lensDecl <- funD funName [lensClause]
  pure (funName, lensDecl)
