{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, 
  ExistentialQuantification, TemplateHaskell, StandaloneDeriving,
  GeneralizedNewtypeDeriving#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Instances are put into this module to avoid circular dependencies
  with the TH module.  There is no need to import this module, since
  it is already re-exported in "Data.Aeson.Diff.Generic".  This module
  is only exported for documentation purpose.
-}

module Data.Aeson.Diff.Generic.Instances
  () where

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
import Data.Semigroup hiding (Sum, Product)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.DList as DList
import Data.Hashable
import Data.Proxy
import Data.Tagged
import Unsafe.Coerce
import Data.Aeson.Diff.Generic.TH
import Data.Aeson.Diff.Generic.Types
import Data.IntMap (IntMap)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Control.Applicative (Const(..))
import Data.Functor.Classes
import Data.Tree (Tree(..))

instance FieldLens Bool
instance FieldLens Char
instance FieldLens Double
instance FieldLens Float
instance FieldLens Int
instance FieldLens Int8
instance FieldLens Int16
instance FieldLens Int32
instance FieldLens Int64
instance FieldLens Integer
instance FieldLens Natural
instance FieldLens Ordering
instance FieldLens Word
instance FieldLens Word8
instance FieldLens Word16
instance FieldLens Word32
instance FieldLens Word64
instance FieldLens ()
instance FieldLens T.Text
instance FieldLens Data.Text.Lazy.Text
instance FieldLens Version
instance FieldLens CTime
instance FieldLens Data.IntSet.IntSet
instance FieldLens Scientific
instance FieldLens LocalTime
instance FieldLens TimeOfDay
instance FieldLens UTCTime
instance FieldLens NominalDiffTime
instance FieldLens DiffTime
instance FieldLens Day
instance FieldLens UUID
instance FieldLens DotNetTime
instance (Hashable a, Eq a, FromJSON a, Typeable a, ToJSON a) =>
         FieldLens (HashSet.HashSet a)
instance (Typeable a, Integral a, ToJSON a, FromJSON a, Eq a)  =>
         FieldLens (Ratio a)
instance (HasResolution a, Typeable a, FromJSON a, ToJSON a) =>
         FieldLens (Fixed a)
instance (JsonPatch a) => FieldLens (IntMap a)
instance Typeable a => FieldLens (Proxy a)

instance JsonPatch Bool
instance JsonPatch Char
instance JsonPatch Double
instance JsonPatch Float
instance JsonPatch Int
instance JsonPatch Int8
instance JsonPatch Int16
instance JsonPatch Int32
instance JsonPatch Int64
instance JsonPatch Integer
instance JsonPatch Natural
instance JsonPatch Ordering
instance JsonPatch Word
instance JsonPatch Word8
instance JsonPatch Word16
instance JsonPatch Word32
instance JsonPatch Word64
instance JsonPatch ()
instance JsonPatch T.Text
instance JsonPatch Data.Text.Lazy.Text
instance JsonPatch Version
instance JsonPatch CTime
instance JsonPatch Data.IntSet.IntSet
instance JsonPatch Scientific
instance JsonPatch LocalTime
instance JsonPatch TimeOfDay
instance JsonPatch UTCTime
instance JsonPatch NominalDiffTime
instance JsonPatch DiffTime
instance JsonPatch Day
instance JsonPatch UUID
instance JsonPatch DotNetTime
-- no indexing possible into hashset, since it is unordered.
instance (Hashable a, Eq a, FromJSON a, Typeable a, ToJSON a) =>
         JsonPatch (HashSet.HashSet a)
instance (Typeable a, Integral a, ToJSON a, FromJSON a, Eq a)  =>
         JsonPatch (Ratio a)
instance (HasResolution a, Typeable a, FromJSON a, ToJSON a) =>
         JsonPatch (Fixed a)
-- IntMap is also a terminal instance, because it is represented by an
-- Array rather than an Object, which makes indexing not possible.
instance JsonPatch a => JsonPatch (IntMap a)         

deriveJsonPatch (defaultOptions {sumEncoding = ObjectWithSingleField}) ''Either
deriveJsonPatch (defaultOptions {sumEncoding = UntaggedValue}) ''Maybe
deriveJsonPatch defaultOptions ''(,)
deriveJsonPatch defaultOptions ''(,,)
deriveJsonPatch defaultOptions ''(,,,)
deriveJsonPatch defaultOptions ''(,,,,)
deriveJsonPatch defaultOptions ''(,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,,,,,)
deriveJsonPatch defaultOptions ''(,,,,,,,,,,,,,,)

instance Typeable a => JsonPatch (Proxy a)
deriving instance JsonPatch a => JsonPatch (Min a)
deriving instance JsonPatch a => JsonPatch (Max a)
deriving instance JsonPatch a => JsonPatch (First a)
deriving instance JsonPatch a => JsonPatch (Last a)
deriving instance JsonPatch a => JsonPatch (WrappedMonoid a)
deriving instance JsonPatch a => JsonPatch (Option a)
deriving instance JsonPatch a => JsonPatch (Identity a)
deriving instance JsonPatch a => JsonPatch (Dual a)
deriving instance (Typeable b, JsonPatch a) => JsonPatch (Const a b)
deriving instance (Eq1 f, Eq1 g, FromJSON1 f, FromJSON1 g, Typeable f,
                   Typeable g, JsonPatch a, ToJSON1 f,
                   ToJSON1 g, JsonPatch (f (g a)),
                   Functor f)
                  => JsonPatch (Compose f g a)
deriving instance (Typeable a, JsonPatch b) => JsonPatch (Tagged a b)

intKey :: Key -> Result Int
intKey (OKey _) = Error "expected Array Key."
intKey (AKey i) = pure i

strKey :: Key -> T.Text
strKey (OKey s) = s
strKey (AKey i) = T.pack $ show i

isEndKey :: Key -> Bool
isEndKey = (== OKey "-")

-- instance FieldLens (Tree a)

splitList :: Int -> [a] -> Maybe ([a], [a])
splitList i _ | i < 0 = Nothing
splitList 0 xs = Just ([], xs)
splitList _ [] = Nothing
splitList n (x:xs) = do
  (l, r) <- splitList (n-1) xs
  pure (x:l, r)

instance (JsonPatch a) => JsonPatch (Tree a)
instance (JsonPatch a) => FieldLens (Tree a) where
  fieldLens key (Node x t) = do
    i <- intKey key
    case i of
      0 -> pure $ GetSet x (\v -> pure $ Node v t)
      1 -> pure $ GetSet t (\v -> pure $ Node x v)
      _ -> Error "Invalid path"

instance (ToJSON1 f, ToJSON1 g, FromJSON1 f, FromJSON1 g, Eq1 f, Eq1 g,
          JsonPatch a, Typeable f, Typeable g, JsonPatch (f a), JsonPatch (g a))
         => JsonPatch (Product f g a)
instance (ToJSON1 f, ToJSON1 g, FromJSON1 f, FromJSON1 g, Eq1 f, Eq1 g,
          JsonPatch a, Typeable f, Typeable g, JsonPatch (f a), JsonPatch (g a))
          => FieldLens (Product f g a) where
  fieldLens key (Pair l r) = do
    i <- intKey key
    case i of
      0 -> pure $ GetSet l (\v -> pure $ Pair v r)
      1 -> pure $ GetSet r (\v -> pure $ Pair l v)
      _ -> Error "Invalid path"

instance (ToJSON1 f, ToJSON1 g, FromJSON1 f, FromJSON1 g, Eq1 f, Eq1 g,
          JsonPatch a, Typeable f, Typeable g, JsonPatch (f a), JsonPatch (g a))
         => JsonPatch (Sum f g a)
instance (ToJSON1 f, ToJSON1 g, FromJSON1 f, FromJSON1 g, Eq1 f, Eq1 g,
          JsonPatch a, Typeable f, Typeable g, JsonPatch (f a), JsonPatch (g a))
          => FieldLens (Sum f g a) where
  fieldLens key (InL x) =
    case strKey key of
      "InL" -> pure $ GetSet x (pure . InL)
      _ -> Error "Invalid path"

  fieldLens key (InR x) =
    case strKey key of
      "InR" -> pure $ GetSet x (pure . InR)
      _ -> Error "Invalid path"

instance JsonPatch a => JsonPatch [a]
instance JsonPatch a => FieldLens [a] where
  fieldLens key lst = do
    i <- intKey key
    case splitList i lst of
      Just (l, r1:rs) ->
        pure $ GetSet r1 (\v -> pure $ l ++ v:rs)
      _ -> Error "Index out of bounds"
  {-# INLINE fieldLens #-}

  insertAt key lst v f
    | isEndKey key = (lst ++) <$> f v
    | otherwise = do
        i <- intKey key
        case splitList i lst of
          Just (l, r) -> (\v' -> l ++ v':r) <$> f v
          Nothing -> Error "Index out of bounds"
  deleteAt key lst f = do
    i <- intKey key
    case splitList i lst of
      Just (l, r1:rs) -> pure (f r1, l ++ rs)
      _ -> Error "Index out of bounds"

instance JsonPatch a => JsonPatch (NonEmpty.NonEmpty a)
instance JsonPatch a => FieldLens (NonEmpty.NonEmpty a) where
  fieldLens key ne = do
    GetSet v f <- fieldLens key (NonEmpty.toList ne)
    pure $ GetSet v (fmap NonEmpty.fromList . f)
  {-# INLINE fieldLens #-}

  insertAt key ne v f =
    NonEmpty.fromList <$> insertAt key (NonEmpty.toList ne) v f
  deleteAt key ne f = do
    (r, l) <- deleteAt key (NonEmpty.toList ne) f
    case NonEmpty.nonEmpty l of
      Nothing -> Error "Cannot delete last element of NonEmpty"
      Just ne2 -> pure (r, ne2)

instance JsonPatch a => JsonPatch (DList.DList a)
instance JsonPatch a => FieldLens (DList.DList a) where
  fieldLens key dl = do
    GetSet v f <- fieldLens key (DList.toList dl)
    pure $ GetSet v (fmap DList.fromList . f)
  {-# INLINE fieldLens #-}

  insertAt key dl v f =
    DList.fromList <$> insertAt key (DList.toList dl) v f
  deleteAt key dl f = 
    fmap DList.fromList <$> deleteAt key (DList.toList dl) f
    
      
instance (Ord a, JsonPatch a) => JsonPatch (Set.Set a) 
instance (Ord a, JsonPatch a) => FieldLens (Set.Set a) where
  fieldLens key st = do
    i <- intKey key
    when (i < 0 || i >= Set.size st) $
      Error "Index out of bounds"
    pure $ GetSet (Set.elemAt i st) $ 
      \v -> pure $ Set.insert v $ Set.deleteAt i st
  {-# INLINE fieldLens #-}
    
  insertAt key st v f
    | isEndKey key =
        (`Set.insert` st) <$> f v
    | otherwise = do
        i <- intKey key
        when (i < 0 || i >= Set.size st) $
          Error "Index out of bounds"
        (`Set.insert` st) <$> f v

  deleteAt key st f = do
    i <- intKey key
    when (i < 0 || i >= Set.size st) $
      Error "Index out of bounds"
    pure (f $ Set.elemAt i st, Set.deleteAt i st)

instance (Ord a, JsonPatch a) => JsonPatch (Seq.Seq a) where
instance (Ord a, JsonPatch a) => FieldLens (Seq.Seq a) where
  fieldLens key sq = do
    i <- intKey key
    case Seq.lookup i sq of
      Nothing -> Error "Index out of bounds"
      Just v -> pure $ GetSet v $
        \v' -> pure $ Seq.update i v' sq
  {-# INLINE fieldLens #-}

  insertAt key sq v f
    | isEndKey key = (sq Seq.|>) <$> f v
    | otherwise = do
        i <- intKey key
        (\v'-> Seq.insertAt i v' sq) <$> f v

  deleteAt key sq f = do
    i <- intKey key
    case Seq.lookup i sq of
      Nothing -> Error "Index out of bounds"
      Just v -> pure (f v, Seq.deleteAt i sq)
  {-# INLINE deleteAt #-}

instance (JsonPatch a) => FieldLens (Vector.Vector a) where
  fieldLens key v = do
    i <- intKey key
    when (i < 0 || i >= Vector.length v) $
      Error "Index out of bounds"
    let (l, r) = Vector.splitAt i v
    pure $ GetSet (Vector.head r) $
      \v' -> pure $ l Vector.++ Vector.cons v' (Vector.tail r)
      
  {-# INLINE fieldLens #-}
  
  insertAt key vec v f
    | isEndKey key = Vector.snoc vec <$> f v
    | otherwise = do
        i <- intKey key
        when (i < 0 || i >= Vector.length vec) $
          Error "Index out of bounds"
        let (l, r) = Vector.splitAt i vec
        (\v' -> l Vector.++ Vector.cons v' r) <$> f v

  deleteAt key vec f = do
    i <- intKey key
    when (i < 0 || i >= Vector.length vec) $
      Error "Index out of bounds"
    let (l, r) = Vector.splitAt i vec
    pure (f $ Vector.head r, l Vector.++ Vector.tail r)

instance (UVector.Unbox a, JsonPatch a) => JsonPatch (UVector.Vector a)
instance (UVector.Unbox a, JsonPatch a) => FieldLens (UVector.Vector a) where
  fieldLens key v = do
    i <- intKey key
    when (i < 0 || i >= UVector.length v) $
      Error "Index out of bounds"
    let (l, r) = UVector.splitAt i v
    pure $ GetSet (UVector.head r) $
      \v' -> pure $ l UVector.++ UVector.cons v' (UVector.tail r)
  {-# INLINE fieldLens #-}

  insertAt key vec v f
    | isEndKey key = UVector.snoc vec <$> f v
    | otherwise = do
        i <- intKey key
        when (i < 0 || i >= UVector.length vec) $
          Error "Index out of bounds"
        let (l, r) = UVector.splitAt i vec
        (\v' -> l UVector.++ UVector.cons v' r) <$> f v

  deleteAt key vec f = do
    i <- intKey key
    when (i < 0 || i >= UVector.length vec) $
      Error "Index out of bounds"
    let (l, r) = UVector.splitAt i vec
    pure (f $ UVector.head r, l UVector.++ UVector.tail r)

instance (SVector.Storable a, JsonPatch a) => JsonPatch (SVector.Vector a) 
instance (SVector.Storable a, JsonPatch a) => FieldLens (SVector.Vector a) where
  fieldLens key v = do
    i <- intKey key
    when (i < 0 || i >= SVector.length v) $
      Error "Index out of bounds"
    let (l, r) = SVector.splitAt i v
    pure $ GetSet (SVector.head r) $
      \v' -> pure $ l SVector.++ SVector.cons v' (SVector.tail r)
  {-# INLINE fieldLens #-}

  insertAt key vec v f
    | isEndKey key = SVector.snoc vec <$> f v
    | otherwise = do
        i <- intKey key
        when (i < 0 || i >= SVector.length vec) $
          Error "Index out of bounds"
        let (l, r) = SVector.splitAt i vec
        (\v' -> l SVector.++ SVector.cons v' r) <$> f v

  deleteAt key vec f = do
    i <- intKey key
    when (i < 0 || i >= SVector.length vec) $
      Error "Index out of bounds"
    let (l, r) = SVector.splitAt i vec
    pure (f $ SVector.head r, l SVector.++ SVector.tail r)

instance (PVector.Prim a, JsonPatch a) => JsonPatch (PVector.Vector a) 
instance (PVector.Prim a, JsonPatch a) => FieldLens (PVector.Vector a) where
  fieldLens key v = do
    i <- intKey key
    when (i < 0 || i >= PVector.length v) $
      Error "Index out of bounds"
    let (l, r) = PVector.splitAt i v
    pure $ GetSet (PVector.head r) $
      \v' -> pure $ l PVector.++ PVector.cons v' (PVector.tail r)
  {-# INLINE fieldLens #-}

  insertAt key vec v f
    | isEndKey key = PVector.snoc vec <$> f v
    | otherwise = do
        i <- intKey key
        when (i < 0 || i >= PVector.length vec) $
          Error "Index out of bounds"
        let (l, r) = PVector.splitAt i vec
        (\v' -> l PVector.++ PVector.cons v' r) <$> f v

  deleteAt key vec f = do
    i <- intKey key
    when (i < 0 || i >= PVector.length vec) $
      Error "Index out of bounds"
    let (l, r) = PVector.splitAt i vec
    pure (f $ PVector.head r, l PVector.++ PVector.tail r)

getMapKey :: FromJSONKey a => T.Text -> Result (Maybe a)
getMapKey s = case fromJSONKey of
  FromJSONKeyCoerce _ -> pure $ Just $ unsafeCoerce s
  FromJSONKeyText fromTxt -> pure $ Just $ fromTxt  s
  FromJSONKeyTextParser parser -> Just <$> parse parser s
  FromJSONKeyValue _ -> pure Nothing

getHashMapKey :: FromJSONKey a => Key -> Result a
getHashMapKey key =
  maybe (Error "Invalid path") pure =<<
  getMapKey (strKey key) 

instance (ToJSONKey k, Typeable k, Eq k, Hashable k, FromJSONKey k, JsonPatch a)
         => JsonPatch (HashMap.HashMap k a)
instance (ToJSONKey k, Typeable k, Eq k, Hashable k, FromJSONKey k, JsonPatch a)
         => FieldLens (HashMap.HashMap k a) where
  fieldLens key hm = do
    k <- getHashMapKey key
    case HashMap.lookup k hm of
      Nothing -> Error "Invalid Pointer"
      Just val ->
        pure $ GetSet val (\v -> pure $ HashMap.insert k v hm)
  {-# INLINE fieldLens #-}

  insertAt key hm v f = do
    k <- getHashMapKey key
    (\v' -> HashMap.insert k v' hm) <$> f v
  
  deleteAt key hm f = do
    k <- getHashMapKey key
    case HashMap.lookup k hm of
      Nothing -> Error "Invalid Pointer"
      Just val -> pure (f val, HashMap.delete k hm)

instance (FromJSONKey k, ToJSONKey k, Eq k, Ord k, JsonPatch k, JsonPatch a)
         => JsonPatch (Map.Map k a)
instance (FromJSONKey k, ToJSONKey k, Eq k, Ord k, JsonPatch a, JsonPatch k)
         => FieldLens (Map.Map k a) where
  fieldLens key map1 = do
    k <- getMapKey $ strKey key
    case k of
      Nothing -> do
        i <- intKey key
        when (i < 0 || i >= Map.size map1) $
          Error "Invalid Pointer"
        let val = Map.elemAt i map1
        pure $ GetSet val
           (\(k2, v) -> pure $ Map.insert k2 v $ 
                        Map.deleteAt i map1)
      Just s ->
        case Map.lookup s map1 of
          Nothing -> Error "Invalid Pointer"
          Just val ->
            pure $ GetSet val (\v -> pure $ Map.insert s v map1)
  {-# INLINE fieldLens #-}

  insertAt key map1 val f = do
    k <- getMapKey $ strKey key
    case k of
      Nothing -> do
        if isEndKey key then pure () else do
          i <- intKey key
          when (i < 0 || i >= Map.size map1) $
            Error "Invalid Pointer"
        (k2, v) <- f val
        pure $ Map.insert k2 v map1
      Just s ->
        (\v -> Map.insert s v map1) <$> f val
        
  deleteAt key map1 f = do
    k <- getMapKey $ strKey key
    case k of
      Nothing -> do
        i <- intKey key
        when (i < 0 || i >= Map.size map1) $
          Error "Invalid Pointer"
        pure (f $ Map.elemAt i map1, Map.deleteAt i map1)
      Just s -> case Map.lookup s map1 of
        Nothing -> Error "Invalid Pointer"
        Just v -> pure (f v, Map.delete s map1)

instance JsonPatch Value where
  getAtPointer ptr val f =
    f <$> Pointer.get ptr val
  deleteAtPointer ptr val f =
    (,) <$> getAtPointer ptr val f <*>
    Diff.applyOperation (Rem ptr) val
  addAtPointer ptr val val2 f = do
    val3 <- f val2
    Diff.applyOperation (Add ptr val3) val
  copyPath from to =
    Diff.applyOperation (Cpy to from)
  movePath from to =
    Diff.applyOperation (Mov to from)
  replaceAtPointer ptr val val2 f = do
    val3 <- f val2
    Diff.applyOperation (Rep ptr val3) val
  testAtPointer ptr val val2 f = do
    val3 <- f val2
    Diff.applyOperation (Tst ptr val3) val

