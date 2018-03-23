{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf, ExistentialQuantification,
    RecordWildCards, TemplateHaskell #-}
module Data.Aeson.Diff.Generic.TH
  where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Control.Monad
import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.List
import Data.Aeson.Diff.Generic.Types

type PathLens s = s -> Path -> Result (Path, Path, GetSet s)

getAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s
                     -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointerFromLens _ (Pointer []) s f = pure $ f s
getAtPointerFromLens l (Pointer path) s f = do
    (_, subPath, GetSet subS _) <- l s path
    getAtPointer (Pointer subPath) subS f

movePathFromLens :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s
                 -> Result s
movePathFromLens _ (Pointer []) (Pointer []) s = pure s
movePathFromLens _ (Pointer []) (Pointer _) _ =
  Error "Cannot move to subpath"
movePathFromLens l (Pointer fromPath) (Pointer toPath) s = do
  (pref, toSubPath, GetSet x setr) <- l s toPath
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
  (_, subPath, GetSet x setr) <- l s path
  setr <$> replaceAtPointer (Pointer subPath) x val f

testAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPointerFromLens _ (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPointerFromLens l (Pointer path) s val f = do
  (_, subPath, GetSet x _) <- l s path
  _ <- testAtPointer (Pointer subPath) x val f
  pure s

data Fake a = Fake1 Int a
            | Fake2 {arg1 :: Int, arg2 :: String}
            | Fake3 a
{-
fakePathLens :: JsonPatch a => PathLens (Fake a) 
fakePathLens x path = case x of
  Fake1 a b -> case path of
    (OKey "contents":key:subPath)
      | key == AKey 0 -> pure ([OKey "contents", AKey 0], subPath, GetSet a (\a2 -> Fake1 a2 b))
      | key == AKey 1 -> pure ([OKey "contents", AKey 1], subPath, GetSet b (\b2 -> Fake1 a b2))
  Fake2 a b -> case path of
    (key:subPath)
      | key == OKey "arg1" -> pure ([OKey "arg1"], subPath, GetSet a (\a2 -> Fake2 a2 b))
      | key == OKey "arg2" -> pure ([OKey "arg2"], subPath, GetSet b (\b2 -> Fake2 a b2))
  Fake3 a -> case path of
    (OKey "contents":subPath) -> pure ([OKey "contents"], subPath, GetSet a (\a2 -> Fake3 a2))
  _ -> Error "invalid path"
-}
  
makePathLens :: Options -> Name -> Q (Name, Dec)
makePathLens options name = do
  typeInfo <- reifyDatatype name
  funName <- newName "pathLens"
  obj <- newName "obj"
  path <- newName "path"
  matches <- mapM (pathLensMatches path "contents" options)
             (datatypeCons typeInfo)
  let body = CaseE (VarE path) matches
      rhs = Clause [VarP obj, VarP path] (NormalB body) []
  pure (funName, FunD funName [rhs])

makeKeyP :: String -> Pat
makeKeyP str = _

makeKeyE :: String -> Exp
makeKeyE str = _

select :: [a] -> [([a], a, [a])]
select [] = []
select l = zip3 (inits l) l (tail $ tails l)

appListE :: ExpQ -> [ExpQ] -> ExpQ
appListE = foldl appE

invalidMatch :: MatchQ
invalidMatch = match wildP (normalB [| Error "Invalid path" |]) []

makePosCases :: Name -> [Exp] -> Name -> [Name] -> ExpQ
makePosCases pathVar prefix consName vs = do
  v2 <- newName "var"
  subPath <- newName "path"
  let mkPosMatch :: ([Name], Name, [Name]) -> Integer -> MatchQ
      mkPosMatch (p, v, n) i =
        match ([p| (AKey $(litP $ integerL i): $(varP subPath)) |])
        (normalB [| pure [ $(pure $ ListE $ prefix ++ [
                                AppE (ConE 'AKey) (LitE $ IntegerL i)])
                         , $(varE subPath)
                         , GetSet $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ] |])
        []
  caseE (varE pathVar) $
    zipWith mkPosMatch (select vs) [0..] ++ [invalidMatch]

makeRecCases :: Name -> [Exp] -> [String] -> [Name] -> Name -> ExpQ
makeRecCases pathVar prefix recordFields vs consName = do
  v2 <- newName "var"
  subPath <- newName "path"
  let mkRecMatch :: ([Name], Name, [Name]) -> String -> MatchQ
      mkRecMatch (p, v, n) fieldName =
        match ([p| $(pure $ makeKeyP fieldName): $(varP subPath) |])
        (normalB [| pure [ $(pure $ ListE $ prefix ++ [makeKeyE fieldName])
                         , $(varE subPath)
                         , GetSet $(varE v)
                           $(lamE [varP v2] $
                             appListE (conE consName) $
                             varE <$> (p ++ v2 : n))
                         ] |])
        []
  caseE (varE pathVar) $
    zipWith mkRecMatch (select vs) recordFields ++ [invalidMatch]

makeSingleCase :: Name -> String -> Name -> Name -> ExpQ
makeSingleCase pathVar prefix v consName = do
  v2 <- newName "var"
  subPath <- newName "path"
  caseE (varE pathVar)
    [ match (conP '(:) [pure $ makeKeyP prefix, varP subPath])
      (normalB [| pure ( [ $(pure $ makeKeyE prefix) ]
                       , $(varE subPath)
                       , GetSet $(varE v)
                         $(lamE [varP v2] $
                           appE (conE consName) $
                           varE v2)) |])
      []
    , invalidMatch]

pathLensMatches :: Name -> String -> Options -> ConstructorInfo -> MatchQ
pathLensMatches pathVar contentsTag options conInfo = do
  case length $ constructorFields conInfo of
    0 -> match (conP (constructorName conInfo) [])
         (normalB [| Error "Invalid path" |]) []
    1 -> _
    n -> do vs <- replicateM n (newName "var")
            let innerCase = caseE (varE pathVar) []
            match (conP (constructorName conInfo) (map varP vs))
              (normalB innerCase) []

{-
 TagedObject:

 pathLens = \x path -> case x of
    Field1 {a::_; b::_; c::_} -> case path of
      (key:path)
        | key == (OKey (fieldLabelModifier "a"):path) -> pure $ ([key], path, GetSet a (\a' -> Field1 a b c))
        | key == (Okey "b":path) -> ...
      _ -> Error "Invalid path"
    Field2 a b c -> case path of
      (contentsFieldName:key:path)
       | key == (AKey 0) -> pure ([contentsFieldName, key], path, GetSet a (\a' -> Field1 a' b c))
        
      _ -> "invalid key"

 OneField a b c -> case path of
      (AKey 0:path) -> ([AKey 0], path, GetSet a (\a' -> Field a' b c))
      ...
     _ -> "invalid key"

    
ObjectWithSingleField:
 fieldLens key x = case case x of
    Field1 {a::_; b::_; c::_} -> case path of
      (OKey constructorTagModifier "Field1": OKey (fieldLabelModifier "a"): path) ->
         GetSet a (\a' -> Field1 a b c)
      _ -> "invalid key: "
    Field2 a b c
      (constructorTagModifier "field2": OKey (fieldLabelModifier "Field2"): path) ->
         GetSet (a, b, c) (\(a', b', c') -> Field2 a' b' c')
      _ -> "invalid key"
 
    OneField a b c

TwoElemArray:
  
  
-}
