{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Data.Aeson.Diff.Generic where
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Patch
import Data.Aeson.Diff
import Data.Aeson.Pointer
import Control.Monad
import Data.Functor.Const
import Data.Functor.Compose
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

type FieldLens s v = forall f.Functor f => (v -> f v) -> f s
type LensConsumer s r = forall v. (JsonPatch v) => FieldLens s v -> Result r

class (Eq s, ToJSON s, FromJSON s, Typeable s) => JsonPatch s where
  fieldLens :: s -> Key -> LensConsumer s r -> Result r
  fieldLens _ _ _ = Error "Invalid pointer"

  deleteAt :: Key -> s -> (forall v.(JsonPatch v) => v -> r)
           -> Result (r, s)
  deleteAt _ _ _ = Error "Illegal operation"

  insertAt :: Key -> s -> r -> (forall v.(JsonPatch v) => r -> Result v)
           -> Result s
  insertAt _ _ _ _ = Error "Illegal operation"

patch :: JsonPatch a => Patch -> a -> Result a
patch = foldr (>=>) pure . map patchOp . patchOperations

patchOp :: JsonPatch a => Operation -> a -> Result a
patchOp op v = case patchOp' op v of
  Error msg -> Error $ opError op ++ msg
  Success ret -> Success ret

opError :: Operation -> String
opError op = mconcat [
  "Operation ", opname op, " on ",
  T.unpack $ formatPointer $ changePointer op,
  " failed: " ]
  where opname (Add _ _) = "Add"
        opname (Cpy _ _) = "Copy"
        opname (Mov _ _) = "Move"
        opname (Rem _)   = "Remove"
        opname (Rep _ _) = "Replace"
        opname (Tst _ _) = "Test"

getDynamic ::(Typeable a) => Dynamic -> Result a
getDynamic = maybe (Error "type mismatch") pure . fromDynamic

getsAtKey :: JsonPatch s => Key -> s -> (forall v.JsonPatch v => v -> Result r) -> Result r
getsAtKey k s f =
  fieldLens s k $ \l ->
  fmap getConst $ getCompose $ l (Compose . fmap Const . f)

updateAtKey :: JsonPatch s => Key -> s -> (forall v.JsonPatch v => v -> Result v) -> Result s
updateAtKey k s f = fieldLens s k (\l -> l f)

setAtKey :: JsonPatch s => Key -> s -> r -> (forall v.JsonPatch v => r -> Result v) -> Result s
setAtKey k s a f = updateAtKey k s $ const $ f a

getAtPointer :: JsonPatch s => Pointer -> s -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointer (Pointer []) s f = pure $ f s
getAtPointer (Pointer (key:path)) s f =
  getsAtKey key s $ \v -> getAtPointer (Pointer path) v f
    
getDynamicAtPointer :: JsonPatch s => Pointer -> s -> Result Dynamic
getDynamicAtPointer p s = getAtPointer p s toDyn

getValueAtPointer :: JsonPatch s => Pointer -> s -> Result Value
getValueAtPointer p s = getAtPointer p s toJSON
  
deleteAtPointer :: JsonPatch s => Pointer -> s ->
                (forall v.JsonPatch v => v -> r) -> Result (r, s)
deleteAtPointer (Pointer []) _ _ = Error "Invalid pointer"
deleteAtPointer (Pointer [key]) s f = deleteAt key s f
deleteAtPointer (Pointer (key:path)) s f =
  fieldLens s key $ \l ->
    getCompose $ l (\v -> Compose $ deleteAtPointer (Pointer path) v f)

addAtPointer :: JsonPatch s => Pointer -> s -> r ->
             (forall v.JsonPatch v => r -> Result v) -> Result s
addAtPointer (Pointer []) _ v f = f v
addAtPointer (Pointer [key]) s val f =
  insertAt key s val f
addAtPointer (Pointer (key:path)) s val f =
  updateAtKey key s (\v -> addAtPointer (Pointer path) v val f)

copyPath :: JsonPatch s => Pointer -> Pointer -> s -> Result s
copyPath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
  | fromKey == toKey =
    updateAtKey toKey s $
    copyPath (Pointer fromPath) (Pointer toPath)
copyPath from to_ s = do
  v <- getDynamicAtPointer from s
  addAtPointer to_ s v getDynamic

movePath :: JsonPatch s => Pointer -> Pointer -> s -> Result s
movePath (Pointer []) (Pointer []) s = pure s
movePath (Pointer []) _ _ = Error "cannot move to child"
movePath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
  | fromKey == toKey =
    updateAtKey toKey s $
    movePath (Pointer fromPath) (Pointer toPath)
movePath from to_ s = do
  (v, s') <- deleteAtPointer from s toDyn
  addAtPointer to_ s' v getDynamic

replaceAtPointer :: JsonPatch s => Pointer -> s -> r ->
             (forall v.JsonPatch v => r -> Result v) -> Result s
replaceAtPointer (Pointer []) _ v f = f v
replaceAtPointer (Pointer [key]) s val f =
  setAtKey key s val f
replaceAtPointer (Pointer (key:path)) s val f =
  updateAtKey key s (\v -> replaceAtPointer (Pointer path) v val f)

testAtPointer :: JsonPatch s => Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPointer (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPointer (Pointer (key:path)) s val f = do
  _ <- updateAtKey key s $ \v ->
    testAtPointer (Pointer path) v val f
  pure s

patchOp' :: JsonPatch a => Operation -> a -> Result a
patchOp' (Add ptr val) s = addAtPointer ptr s val fromJSON
patchOp' (Rem ptr) s = snd <$> deleteAtPointer ptr s (const ())
patchOp' (Cpy toPath fromPath) s = copyPath fromPath toPath s
patchOp' (Mov toPath fromPath) s = movePath fromPath toPath s
patchOp' (Rep ptr val) s = replaceAtPointer ptr s val fromJSON
patchOp' (Tst ptr val) s = testAtPointer ptr s val fromJSON
            
intKey :: Key -> Result Int
intKey (OKey _) = Error "expected Array Key."
intKey (AKey i) = pure i

strKey :: Key -> String
strKey (OKey s) = T.unpack s
strKey (AKey i) = show i

isEndKey :: Key -> Bool
isEndKey = (== OKey "-")

newtypeFieldLens :: (JsonPatch u, JsonPatch w) => (u -> w) -> (w -> u)
                 -> w -> Key -> LensConsumer w r -> Result r
newtypeFieldLens wrap unwrap el key cont = 
  fieldLens (unwrap el) key $ \l -> cont $ fmap wrap . l
{-# INLINE newtypeFieldLens #-}
  
newtypeInsertAt :: (JsonPatch u, JsonPatch w) => (u -> w) -> (w -> u)
                -> Key -> w -> r ->  (forall v.(JsonPatch v) => r -> Result v)
                -> Result w
newtypeInsertAt wrap unwrap key el r f = 
  wrap <$> insertAt key (unwrap el) r f
{-# INLINE newtypeInsertAt #-}  

newtypeDeleteAt :: (JsonPatch u, JsonPatch w) => (u -> w) -> (w -> u)
                -> Key -> w -> (forall v.(JsonPatch v) => v -> r)
                -> Result (r, w)
newtypeDeleteAt wrap unwrap key el f = 
  fmap wrap <$> deleteAt key (unwrap el) f
{-# INLINE newtypeDeleteAt #-}    

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

instance JsonPatch a => JsonPatch (Min a) where
  fieldLens = newtypeFieldLens Min getMin
  insertAt = newtypeInsertAt Min getMin
  deleteAt = newtypeDeleteAt Min getMin

instance JsonPatch a => JsonPatch (Max a) where
  fieldLens = newtypeFieldLens Max getMax
  insertAt = newtypeInsertAt Max getMax
  deleteAt = newtypeDeleteAt Max getMax

instance JsonPatch a => JsonPatch (First a) where
  fieldLens = newtypeFieldLens First getFirst
  insertAt = newtypeInsertAt First getFirst
  deleteAt = newtypeDeleteAt First getFirst

instance JsonPatch a => JsonPatch (Last a) where
  fieldLens = newtypeFieldLens Last getLast
  insertAt = newtypeInsertAt Last getLast
  deleteAt = newtypeDeleteAt Last getLast

instance JsonPatch a => JsonPatch (WrappedMonoid a) where
  fieldLens = newtypeFieldLens WrapMonoid unwrapMonoid
  insertAt = newtypeInsertAt WrapMonoid unwrapMonoid
  deleteAt = newtypeDeleteAt WrapMonoid unwrapMonoid

instance JsonPatch a => JsonPatch (Option a) where
  fieldLens = newtypeFieldLens Option getOption
  insertAt = newtypeInsertAt Option getOption
  deleteAt = newtypeDeleteAt Option getOption

instance JsonPatch a => JsonPatch (Identity a) where
  fieldLens = newtypeFieldLens Identity runIdentity
  insertAt = newtypeInsertAt Identity runIdentity
  deleteAt = newtypeDeleteAt Identity runIdentity

instance JsonPatch a => JsonPatch (Dual a) where
  fieldLens = newtypeFieldLens Dual getDual
  insertAt = newtypeInsertAt Dual getDual
  deleteAt = newtypeDeleteAt Dual getDual

instance (Typeable a, JsonPatch b) => JsonPatch (Tagged a b) where
  fieldLens = newtypeFieldLens Tagged unTagged
  insertAt = newtypeInsertAt Tagged unTagged
  deleteAt = newtypeDeleteAt Tagged unTagged
  
instance (JsonPatch a, JsonPatch b) => JsonPatch (Either a b)
instance JsonPatch a => JsonPatch (Data.List.NonEmpty.NonEmpty a)
instance JsonPatch a => JsonPatch (Maybe a)
instance JsonPatch Value where
instance (JsonPatch a, JsonPatch b) => JsonPatch (a, b)
-- instance JsonPatch (Map a)
-- instance JsonPatch (Tree a)
-- instance JsonPatch Product
-- instance JsonPatch Sum
-- instance JsonPatch Compose

splitList :: Int -> [a] -> Maybe ([a], [a])
splitList i _ | i < 0 = Nothing
splitList 0 xs = Just ([], xs)
splitList _ [] = Nothing
splitList n (x:xs) = do
  (l, r) <- splitList (n-1) xs
  pure (x:l, r)

instance JsonPatch a => JsonPatch [a] where
  fieldLens lst key cont = do
    i <- intKey key
    case splitList i lst of
      Just (l, r1:rs) ->
        cont $ \f -> (\v -> l ++ v:rs) <$> f r1
      _ -> Error "Index out of bounds"

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

instance (Ord a, JsonPatch a) => JsonPatch (Set.Set a) where
  fieldLens st key cont = do
    i <- intKey key
    when (i < 0 || i >= Set.size st) $
      Error "Index out of bounds"
    cont $ \f -> (\v -> Set.insert v $ Set.deleteAt i st)
                 <$> f (Set.elemAt i st)
    
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
  fieldLens sq key cont = do
    i <- intKey key
    case Seq.lookup i sq of
      Nothing -> Error "Index out of bounds"
      Just v ->
        cont $ \f ->
        (\v' -> Seq.update i v' sq) <$> f v

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

-- instance (Ord a, JsonPatch a) => JsonPatch (Tree.Tree a) where
--   fieldLens (Tree.Node a forest) key cont = do
--     i <- intKey key
--     case i of
--       0 -> cont $ \f -> flip Tree.Node forest <$> f a
--       1 -> cont $ \f -> Tree.Node a <$> f forest
--       _ -> Error "Index out of bounds"
    
--   insertAt _ _ _ _ = Error "Illegal operation"
--   deleteAt _ _ _ = Error "Illegal operation"

instance (JsonPatch a) => JsonPatch (Vector.Vector a) where
  fieldLens v key cont = do
    i <- intKey key
    when (i < 0 || i >= Vector.length v) $
      Error "Index out of bounds"
    let (l, r) = Vector.splitAt i v
    cont $ \f ->
      (\v' -> l Vector.++ Vector.cons v' (Vector.tail r)) <$>
      f (Vector.head r)

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

instance (UVector.Unbox a, JsonPatch a) => JsonPatch (UVector.Vector a) where
  fieldLens v key cont = do
    i <- intKey key
    when (i < 0 || i >= UVector.length v) $
      Error "Index out of bounds"
    let (l, r) = UVector.splitAt i v
    cont $ \f ->
      (\v' -> l UVector.++ UVector.cons v' (UVector.tail r)) <$>
      f (UVector.head r)

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

instance (SVector.Storable a, JsonPatch a) => JsonPatch (SVector.Vector a) where
  fieldLens v key cont = do
    i <- intKey key
    when (i < 0 || i >= SVector.length v) $
      Error "Index out of bounds"
    let (l, r) = SVector.splitAt i v
    cont $ \f ->
      (\v' -> l SVector.++ SVector.cons v' (SVector.tail r)) <$>
      f (SVector.head r)

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

instance (PVector.Prim a, JsonPatch a) => JsonPatch (PVector.Vector a) where
  fieldLens v key cont = do
    i <- intKey key
    when (i < 0 || i >= PVector.length v) $
      Error "Index out of bounds"
    let (l, r) = PVector.splitAt i v
    cont $ \f ->
      (\v' -> l PVector.++ PVector.cons v' (PVector.tail r)) <$>
      f (PVector.head r)

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
  getMapKey (T.pack $ strKey key) 

instance (ToJSONKey k, Typeable k, Eq k, Hashable k, FromJSONKey k, JsonPatch a)
         => JsonPatch (HashMap.HashMap k a) where
  fieldLens hm key cont = do
    k <- getHashMapKey key
    case HashMap.lookup k hm of
      Nothing -> Error "Invalid Pointer"
      Just val ->
        cont $ \f -> (\v -> HashMap.insert k v hm) <$> f val

  insertAt key hm v f = do
    k <- getHashMapKey key
    (\v' -> HashMap.insert k v' hm) <$> f v
  deleteAt key hm f = do
    k <- getHashMapKey key
    case HashMap.lookup k hm of
      Nothing -> Error "Invalid Pointer"
      Just val -> pure (f val, HashMap.delete k hm)

