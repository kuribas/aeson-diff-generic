{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aeson.Diff.Generic.PathOptics
  ( -- * type synonyms
    PathLens, PathTraversal,
    -- * operations over path optics
    overPath, replacePath, getPath, appendPath,
    insertPath, deletePath, testPath,
    KeyIndexed(..), Insertable(..), Appendable(..),
    -- * creating and extracting path optics
    withPath, withoutPath,
    -- * pathOptics
    ixP, traverseP, _1P, _2P, _3P, _4P, _5P,
    _6P, _7P, _8P, _9P, _10P, _11P, _12P, _13P,
    _14P, _15P, _16P, _17P, _18P, _19P, _LeftP, _RightP,
    _JustP)

where
import Data.Aeson
import Data.Aeson.Diff
import Data.Aeson.Pointer
import Data.Functor.Compose
import Control.Lens

-- base >= 4.8: `Monoid` class is exported via `Prelude`
-- base < 4.11: re-exports `Monoid` class & common newtype wrappers
-- base >= 4.11: doesn't reexport `Monoid` class anymore
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

type PathTraversal s a = Traversal' (Path, s) (Path, a)
type PathLens s a = Lens' (Path, s) (Path, a)

class KeyIndexed s where
  toKey :: Index s -> s -> Key

class KeyIndexed a => Insertable a where
  insertKey :: Index a -> (IxValue a) -> a -> a
  deleteKey :: Index a -> a -> Maybe (IxValue a, a)

class KeyIndexed a => Appendable a where
  append :: (IxValue a) -> a -> a

-- | Annotate an optic with a path.  The output optic will be the same
-- optic, with a path annotation.  For example:
--
-- > withPath :: Path -> Lens' s a -> Lens' (Path, s) (Path, a)
-- > withPath :: Path -> Traversal' s a -> Traversal' (Path, s) (Path, a)

withPath ::  Functor f 
         => Path -- ^ the path into the structure
         -> ((a -> Compose f ((,) Path) a) -> (s -> Compose f ((,) Path) s))
         -- ^ the input optic
         -> (((Path, a) -> f (Path, a)) ->  (Path, s) -> f (Path, s))
         -- ^ the output optic with path information
withPath p2 orig f (p, v) =
  fmap (over _1 (p <>)) $ getCompose $
  orig (\a -> Compose $ f (p2,a)) v

-- | remove the path from an optic annotated with path information.
-- 
-- > withoutPath . withPath ≡ id
--
-- The output optic will be the same
-- optic, without the path annotation.  For example:
--
-- > withoutPath ::  Lens' (Path, s) (Path, a) -> Lens' s a
-- > withoutPath :: Traversal' (Path, s) (Path, a) -> Traversal' s a
withoutPath :: Functor f
            => (((Path, a) -> f (Path, a)) -> ((Path, s) -> f (Path, s)))
            -- ^ input optic
            -> ((a -> f a) -> (s -> f s))
            -- ^ output optic without path information
withoutPath l f v = snd <$> l (traverseOf _2 f) (rootPath, v)

-- | the root path
rootPath :: Path
rootPath = []

-- | Modify the value(s) at the traversal, and return a patch which
-- represents those (replace) operations.
overPath :: ToJSON a => PathTraversal s a -> (a -> a) -> s -> (Patch, s)
overPath lns f s =
  snd <$>
  lns (\(p2, a) ->
         let a2 = f a in
           ( Patch [Rep (Pointer p2) (toJSON a2)]
           , (p2, a2)))
    (rootPath, s)
    
-- | Replace the value at the traversal, and return a patch with
--  the replace operations.
replacePath :: ToJSON a => PathTraversal s a -> a -> s -> (Patch, s)
replacePath lns = overPath lns . const

-- | Return the values and pointers pointed to by the traversal (if any)
getPath :: PathTraversal s a -> s -> [(Pointer, a)]
getPath lns v = over _1 Pointer <$> (rootPath, v) ^.. lns

-- | Append a value to the structure pointed by the lens.  Return a
-- patch which the append operations)
appendPath :: (ToJSON (IxValue a), Appendable a)
           => IxValue a -> PathTraversal s a -> s -> (Patch, s)
appendPath v lns s =
  snd <$>
  lns (\(p2, a) ->
         let a2 = append v a
         in ( Patch [Add (Pointer $ p2 ++ [OKey "-"]) (toJSON v)]
            , (p2, a2)))
    (rootPath, s)

-- | Insert a value at the given key, and return a patch with the
-- insert operations
insertPath :: (ToJSON (IxValue a), Insertable a)
           => Index a -> IxValue a -> PathTraversal s a -> s
           -> (Patch, s)
insertPath key v lns s =
  fmap snd $
  lns (\(p2, a) ->
         let a2 = insertKey key v a
         in ( Patch [Add (Pointer $ p2 <>
                           [toKey key a]) (toJSON v)]
            , (p2, a2)))
    (rootPath, s)  

-- | Delete a value at the given key if present, and return a patch
-- with the delete operations.
deletePath :: (ToJSON (IxValue a), Insertable a)
           => Index a -> PathTraversal s a -> s
           -> (Patch, s)
deletePath key lns s =
  fmap snd $
  lns (\(p2, a) ->
         case deleteKey key a of
           Nothing -> (Patch [], (p2, a))
           Just (_, a2) ->
             ( Patch [Rem (Pointer $ p2 <> [toKey key a])]
             , (p2, a2)))
    (rootPath, s)  

-- | Create a patch that tests if the value at the pointer equals the
-- current value.  Returns the data unmodified.
testPath :: ToJSON a => PathTraversal s a -> s -> (Patch, s)
testPath lns s =
  ( Patch $ (\(ptr, v) -> Tst ptr (toJSON v)) <$> getPath lns s
  , s)

-- | `ix` from lens annotated with a path.  Provides a simple
-- Traversal lets you traverse the value at a given key in a Map or
-- element at an ordinal position in a list or Seq.  Includes the path
-- to the element.
ixP :: (KeyIndexed s, Ixed s) =>
       Index s -> Traversal' (Path, s) (Path, IxValue s)
ixP i f s = (withPath [toKey i (snd s)] (ix i)) f s

traverseP :: (a ~ Index (s a), TraversableWithIndex a s, 
              KeyIndexed (s a))
          => Traversal' (Path, (s a)) (Path, a)
traverseP f (p, s) =
  fmap (over _1 (p <>)) $ getCompose $
  itraverse (\i a -> Compose (f ([toKey i s], a))) s

_1P :: Field1 s s a a  => PathLens s a
_1P = withPath [AKey 0] _1

_2P :: Field2 s s a a  => PathLens s a
_2P = withPath [AKey 1] _2

_3P :: Field3 s s a a  => PathLens s a
_3P = withPath [AKey 2] _3

_4P :: Field4 s s a a  => PathLens s a
_4P = withPath [AKey 3] _4

_5P :: Field5 s s a a  => PathLens s a
_5P = withPath [AKey 4] _5

_6P :: Field6 s s a a  => PathLens s a
_6P = withPath [AKey 5] _6

_7P :: Field7 s s a a  => PathLens s a
_7P = withPath [AKey 6] _7

_8P :: Field8 s s a a  => PathLens s a
_8P = withPath [AKey 7] _8

_9P :: Field9 s s a a  => PathLens s a
_9P = withPath [AKey 8] _9

_10P :: Field10 s s a a  => PathLens s a
_10P = withPath [AKey 9] _10

_11P :: Field11 s s a a  => PathLens s a
_11P = withPath [AKey 10] _11

_12P :: Field12 s s a a  => PathLens s a
_12P = withPath [AKey 11] _12

_13P :: Field13 s s a a  => PathLens s a
_13P = withPath [AKey 12] _13

_14P :: Field14 s s a a  => PathLens s a
_14P = withPath [AKey 13] _14

_15P :: Field15 s s a a  => PathLens s a
_15P = withPath [AKey 14] _15

_16P :: Field16 s s a a  => PathLens s a
_16P = withPath [AKey 15] _16

_17P :: Field17 s s a a  => PathLens s a
_17P = withPath [AKey 16] _17

_18P :: Field18 s s a a  => PathLens s a
_18P = withPath [AKey 17] _18

_19P :: Field19 s s a a  => PathLens s a
_19P = withPath [AKey 18] _19

_LeftP :: Traversal' (Path, Either a b) (Path, a)
_LeftP = withPath [OKey "Left"] _Left

_RightP :: Traversal' (Path, Either a b) (Path, b)
_RightP = withPath [OKey "Right"] _Right

_JustP :: Traversal' (Path, Maybe a) (Path, a)
_JustP = withPath [] _Just
