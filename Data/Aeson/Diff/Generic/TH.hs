{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf, ExistentialQuantification,
    RecordWildCards, TemplateHaskell #-}
module Data.Aeson.Diff.Generic.TH
  where

import Data.Aeson.Types
import Data.Aeson.Patch
import qualified Data.Aeson.Diff as Diff
import Data.Aeson.Pointer as Pointer
import Control.Monad
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Lazy
import Data.Dynamic
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Numeric.Natural
import Data.Version
import Foreign.C.Types
import qualified Data.IntSet
import Data.Scientific
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.UUID.Types
import Data.Ratio
import Data.Fixed
import Data.Semigroup
import qualified Data.List.NonEmpty
import qualified Data.DList
import Data.Hashable
import Data.Proxy
import Data.Tagged
import Unsafe.Coerce
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.Traversable
import Data.List
import Data.Aeson.Diff.Generic.Types
import Data.Aeson.Diff.Generic.Instances

type PathLens s = s -> Path -> Result (Path, Path, GetSet s)

getAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointerFromLens l (Pointer []) s f = pure $ f s
getAtPointerFromLens l (Pointer path) s f = do
    (path1, path2, GetSet s2 _) <- l s path
    getAtPointer (Pointer path) s2 f

movePathFromLens :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s -> Result s
movePathFromLens l (Pointer []) (Pointer toPath) s = _
movePathFromLens l (Pointer fromPath) (Pointer toPath) s = do
  (pref, toPath2, GetSet x setr) <- l s toPath
  if pref `isPrefixOf` toPath
    then setr <$> movePath (Pointer (take (length pref) fromPath)) (Pointer toPath2) x
    else do (v, s') <- deleteAtPointer (Pointer fromPath) s toDyn
            addAtPointer (Pointer toPath) s' v getDynamic

copyPathFromLens :: (JsonPatch s) => PathLens s -> Pointer -> Pointer -> s -> Result s
copyPathFromLens l (Pointer []) (Pointer toPath) s = _
copyPathFromLens l (Pointer fromPath) (Pointer toPath) s = do
  (pref, toPath2, GetSet x setr) <- l s toPath
  v <- getAtPointer (Pointer fromPath) s toDyn
  addAtPointer (Pointer toPath) s v getDynamic

replaceAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
                            (forall v.JsonPatch v => r -> Result v) -> Result s
replaceAtPointerFromLens _ (Pointer []) _ v f = f v
replaceAtPointerFromLens l (Pointer path) s val f = do
  (_, path2, GetSet x setr) <- l s path
  setr <$> replaceAtPointer (Pointer path2) x val f

testAtPointerFromLens :: (JsonPatch s) => PathLens s -> Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPointerFromLens _ (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPointerFromLens l (Pointer (key:path)) s val f = do
  (_, path2, GetSet x setr) <- l s path
  testAtPointer (Pointer path) x val f
  pure s

data Fake a = Fake1 Int a
            | Fake2 {arg1 :: Int, arg2 :: String}
            | Fake3 a

fakePathLens :: JsonPatch a => PathLens (Fake a) 
fakePathLens x path = case x of
  Fake1 a b -> case path of
    (OKey "contents":key:path2)
      | key == AKey 0 -> pure ([OKey "contents", AKey 0], path2, GetSet a (\a2 -> Fake1 a2 b))
      | key == AKey 1 -> pure ([OKey "contents", AKey 1], path2, GetSet b (\b2 -> Fake1 a b2))
  Fake2 a b -> case path of
    (key:path2)
      | key == OKey "arg1" -> pure ([OKey "arg1"], path2, GetSet a (\a2 -> Fake2 a2 b))
      | key == OKey "arg2" -> pure ([OKey "arg2"], path2, GetSet b (\b2 -> Fake2 a b2))
  Fake3 a -> case path of
    (OKey "contents":path2) -> pure ([OKey "contents"], path2, GetSet a (\a2 -> Fake3 a2))
  _ -> Error "invalid path"
  
makePathLens :: Options -> Name -> Q (Name, Dec)
makePathLens options name = do
  typeInfo <- reifyDatatype name
  funName <- newName "pathLens"
  obj <- newName "obj"
  path <- newName "path"
  matches <- mapM (pathLensMatches path options) (datatypeCons typeInfo)
  let body = CaseE (VarE path) matches
      rhs = Clause [VarP obj, VarP path] (NormalB body) []
  pure (funName, FunD funName [rhs])

pathLensMatches :: Name -> Options -> ConstructorInfo -> Q Match  
pathLensMatches path options conInfo =
  case length $ constructorFields conInfo of
    0 -> match (conP (constructorName conInfo) [])
         (normalB [| Error "Invalid path" |]) []
    1 -> do v <- newName "var"
            match (conP (constructorName conInfo) [varP v])
              (normalB [| Error "" |]) []
    n -> do vs <- replicateM n (newName "var")
            let innerCase = caseE (varE path) []
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
