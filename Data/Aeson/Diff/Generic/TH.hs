{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf, ExistentialQuantification #-}
module Data.Aeson.Diff.Generic.TH
  (deriveFieldLensHiding) where

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

deriveFieldLensHiding :: Options -> [String] -> Name -> DecsQ
deriveFieldLensHiding options exclude name = _

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

{-
 TagedObject:

 fieldLens key x = case x of
    Field1 {a::_; b::_; c::_} -> case key of
      OKey (fieldLabelModifier "a") -> pure $ GetSet a (\a' -> Field1 a b c)
      Okey "b" -> ...
      _ -> Error "invalid key: "
    Field2 a b c -> case key of
      contentsFieldName -> let
        f (AKey 0) = pure $ GetSet a (\a' -> Field1 a' b c)
        _   
        content = ObjectContents key c f where
        in GetSet content getObjectContents
      _ -> "invalid key"
 

    OneField a b c -> case key of
      AKey 0 -> GetSet a (\a' -> Field a' b c)
      ...
     _ -> "invalid key"

    
ObjectWithSingleField:
 fieldLens key x = case x of
    Field1 {a::_; b::_; c::_}
      | key == OKey (fieldLabelModifier "Field1") ->
         GetSet a (\a' -> Field1 a b c)
      _ -> "invalid key: "
    Field2 a b c
      | key == OKey (fieldLabelModifier "Field2") ->
         GetSet (a, b, c) (\(a', b', c') -> Field2 a' b' c')
      _ -> "invalid key"
 
    OneField a b c
-}
