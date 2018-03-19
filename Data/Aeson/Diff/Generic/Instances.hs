{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.Semigroup
import qualified Data.List.NonEmpty
import qualified Data.DList
import Data.Hashable
import Data.Proxy
import Data.Tagged
import Unsafe.Coerce
import Language.Haskell.TH
import Data.Aeson.Diff.Generic.Types

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
instance (Eq a, FromJSON a, Typeable a, ToJSON a) => JsonPatch (Data.DList.DList a)
instance (Hashable a, Eq a, FromJSON a, Typeable a, ToJSON a) => JsonPatch (HashSet.HashSet a)
instance (Typeable a, Integral a, ToJSON a, FromJSON a, Eq a)  => JsonPatch (Ratio a)
instance (HasResolution a, Typeable a, FromJSON a, ToJSON a) => JsonPatch (Fixed a)
instance (Typeable a) => JsonPatch (Proxy a)
instance (FieldLens a) => JsonPatch (Min a)
instance FieldLens a => JsonPatch (Max a)
instance FieldLens a => JsonPatch (First a)
instance FieldLens a => JsonPatch (Last a)
instance FieldLens a => JsonPatch (WrappedMonoid a)
instance (FieldLens a, JsonPatch a) => JsonPatch (Option a)
instance FieldLens a => JsonPatch (Identity a)
instance FieldLens a => JsonPatch (Dual a)
instance (FieldLens b, Typeable a) => JsonPatch (Tagged a b)
instance (JsonPatch a, JsonPatch b) => JsonPatch (Either a b)
instance JsonPatch a => JsonPatch (Data.List.NonEmpty.NonEmpty a)
instance JsonPatch a => JsonPatch (Maybe a)
instance (JsonPatch a, JsonPatch b) => JsonPatch (a, b)

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
instance (Eq a, FromJSON a, Typeable a, ToJSON a) => FieldLens (Data.DList.DList a)
instance (Hashable a, Eq a, FromJSON a, Typeable a, ToJSON a) => FieldLens (HashSet.HashSet a)
instance (Typeable a, Integral a, ToJSON a, FromJSON a, Eq a)  => FieldLens (Ratio a)
instance (HasResolution a, Typeable a, FromJSON a, ToJSON a) => FieldLens (Fixed a)
instance (Typeable a) => FieldLens (Proxy a)

instance (FieldLens a) => FieldLens (Min a) where
  fieldLens = newtypeFieldLens Min getMin
  insertAt = newtypeInsertAt Min getMin
  deleteAt = newtypeDeleteAt Min getMin

instance FieldLens a => FieldLens (Max a) where
  fieldLens = newtypeFieldLens Max getMax
  insertAt = newtypeInsertAt Max getMax
  deleteAt = newtypeDeleteAt Max getMax

instance FieldLens a => FieldLens (First a) where
  fieldLens = newtypeFieldLens First getFirst
  insertAt = newtypeInsertAt First getFirst
  deleteAt = newtypeDeleteAt First getFirst

instance FieldLens a => FieldLens (Last a) where
  fieldLens = newtypeFieldLens Last getLast
  insertAt = newtypeInsertAt Last getLast
  deleteAt = newtypeDeleteAt Last getLast

instance FieldLens a => FieldLens (WrappedMonoid a) where
  fieldLens = newtypeFieldLens WrapMonoid unwrapMonoid
  insertAt = newtypeInsertAt WrapMonoid unwrapMonoid
  deleteAt = newtypeDeleteAt WrapMonoid unwrapMonoid

instance (FieldLens a, JsonPatch a) => FieldLens (Option a) where
  fieldLens = newtypeFieldLens Option getOption
  insertAt = newtypeInsertAt Option getOption
  deleteAt = newtypeDeleteAt Option getOption

instance FieldLens a => FieldLens (Identity a) where
  fieldLens = newtypeFieldLens Identity runIdentity
  insertAt = newtypeInsertAt Identity runIdentity
  deleteAt = newtypeDeleteAt Identity runIdentity

instance FieldLens a => FieldLens (Dual a) where
  fieldLens = newtypeFieldLens Dual getDual
  insertAt = newtypeInsertAt Dual getDual
  deleteAt = newtypeDeleteAt Dual getDual

instance (FieldLens b, Typeable a) => FieldLens (Tagged a b) where
  fieldLens = newtypeFieldLens Tagged unTagged
  insertAt = newtypeInsertAt Tagged unTagged
  deleteAt = newtypeDeleteAt Tagged unTagged


intKey :: Key -> Result Int
intKey (OKey _) = Error "expected Array Key."
intKey (AKey i) = pure i

strKey :: Key -> T.Text
strKey (OKey s) = s
strKey (AKey i) = T.pack $ show i

isEndKey :: Key -> Bool
isEndKey = (== OKey "-")

instance (JsonPatch a, JsonPatch b) => FieldLens (Either a b)
instance JsonPatch a => FieldLens (Data.List.NonEmpty.NonEmpty a)
instance JsonPatch a => FieldLens (Maybe a)

instance (JsonPatch a, JsonPatch b) => FieldLens (a, b)
-- instance FieldLens (Map a)
-- instance FieldLens (Tree a)
-- instance FieldLens Product
-- instance FieldLens Sum
-- instance FieldLens Compose

splitList :: Int -> [a] -> Maybe ([a], [a])
splitList i _ | i < 0 = Nothing
splitList 0 xs = Just ([], xs)
splitList _ [] = Nothing
splitList n (x:xs) = do
  (l, r) <- splitList (n-1) xs
  pure (x:l, r)

instance JsonPatch a => JsonPatch [a]
instance JsonPatch a => FieldLens [a] where
  fieldLens lst key = do
    i <- intKey key
    case splitList i lst of
      Just (l, r1:rs) ->
        pure $ GetSet r1 (\v -> l ++ v:rs)
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

instance (Ord a, JsonPatch a) => JsonPatch (Set.Set a) 
instance (Ord a, JsonPatch a) => FieldLens (Set.Set a) where
  fieldLens st key = do
    i <- intKey key
    when (i < 0 || i >= Set.size st) $
      Error "Index out of bounds"
    pure $ GetSet (Set.elemAt i st) (\v -> Set.insert v $ Set.deleteAt i st)
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
  fieldLens sq key = do
    i <- intKey key
    case Seq.lookup i sq of
      Nothing -> Error "Index out of bounds"
      Just v -> pure $ GetSet v (\v' -> Seq.update i v' sq)
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
  fieldLens v key = do
    i <- intKey key
    when (i < 0 || i >= Vector.length v) $
      Error "Index out of bounds"
    let (l, r) = Vector.splitAt i v
    pure $ GetSet (Vector.head r) (\v' -> l Vector.++ Vector.cons v' (Vector.tail r))
      
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
  fieldLens v key = do
    i <- intKey key
    when (i < 0 || i >= UVector.length v) $
      Error "Index out of bounds"
    let (l, r) = UVector.splitAt i v
    pure $ GetSet (UVector.head r)
      (\v' -> l UVector.++ UVector.cons v' (UVector.tail r))
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
  fieldLens v key = do
    i <- intKey key
    when (i < 0 || i >= SVector.length v) $
      Error "Index out of bounds"
    let (l, r) = SVector.splitAt i v
    pure $ GetSet (SVector.head r)
      (\v' -> l SVector.++ SVector.cons v' (SVector.tail r))
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
  fieldLens v key = do
    i <- intKey key
    when (i < 0 || i >= PVector.length v) $
      Error "Index out of bounds"
    let (l, r) = PVector.splitAt i v
    pure $ GetSet (PVector.head r)
      (\v' -> l PVector.++ PVector.cons v' (PVector.tail r))
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
  fieldLens hm key = do
    k <- getHashMapKey key
    case HashMap.lookup k hm of
      Nothing -> Error "Invalid Pointer"
      Just val ->
        pure $ GetSet val (\v -> HashMap.insert k v hm)
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
  fieldLens map1 key = do
    k <- getMapKey $ strKey key
    case k of
      Nothing -> do
        i <- intKey key
        when (i < 0 || i >= Map.size map1) $
          Error "Invalid Pointer"
        let val = Map.elemAt i map1
        pure $ GetSet val
           (\(k2, v) -> Map.insert k2 v $ 
                        Map.deleteAt i map1)
      Just s ->
        case Map.lookup s map1 of
          Nothing -> Error "Invalid Pointer"
          Just val ->
            pure $ GetSet val (\v -> Map.insert s v map1)
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
