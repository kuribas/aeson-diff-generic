{-# LANGUAGE OverloadedStrings, RankNTypes, TypeFamilies, FlexibleContexts #-}
module Data.Aeson.Diff.Generic where
import Data.Aeson
import Data.Aeson.Patch
import Data.Aeson.Diff
import Data.Aeson.Pointer
import Control.Monad 
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.ByteString.Lazy (ByteString)
import Data.Dynamic
import Data.Functor.Identity
import Data.Functor.Const

class JsonPatch a where
  patchOp :: Operation -> a -> Result a
  valueAt :: Pointer -> a -> Result Value
  encodeAt :: Pointer -> a -> Result ByteString
  deleteAt :: Pointer -> a -> Result (Dynamic, a)
  addAt :: Pointer -> Dynamic -> a -> Result a
  
patch :: JsonPatch a => Patch -> a -> Result a
patch = foldr (>=>) pure . map patchOp' . patchOperations

patchOp' :: JsonPatch a => Operation -> a -> Result a
patchOp' op v = case patchOp op v of
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
        opname (Rem _) = "Remove"
        opname (Rep _ _) = "Replace"
        opname (Tst _ _) = "Test"
        
intKey :: Key -> Result Int
intKey (OKey _) = Error "expected Array Key."
intKey (AKey i) = pure i

strKey :: Key -> String
strKey (OKey s) = T.unpack s
strKey (AKey i) = show i

isEndKey :: Key -> Bool
isEndKey = (== OKey "-")

class ArrayLike a where
  type ArrayVal a :: *
  arrAppend :: a -> ArrayVal a -> a
  arrInsert :: Int -> ArrayVal a -> a -> Result a
  arrDelete :: Int -> a -> Result (ArrayVal a, a)
  arrLens :: Functor f => Int -> (ArrayVal a -> Result (f (ArrayVal a)))
             -> (a -> Result (f a))

arrGet :: (ArrayLike a) =>  Int -> a -> Result (ArrayVal a)
arrGet i = fmap getConst . arrLens i (pure . Const)

arrUpdate :: (ArrayLike a) =>  Int -> (ArrayVal a -> Result (ArrayVal a)) -> a -> Result a
arrUpdate i f =
  fmap runIdentity . arrLens i (fmap Identity . f)

arrPut :: (ArrayLike a) => Int -> Result (ArrayVal a) -> a -> Result a
arrPut i = arrUpdate i . const

class ObjectLike a where
  type ObjectVal a :: *
  objInsert :: String -> v -> a -> Result a
  objDelete :: String -> a -> Result (ObjectVal a, a)
  objLens :: String -> (ObjectVal a -> Result (f (ObjectVal a)))
          -> (a -> Result (f a))

objGet :: (ObjectLike a) =>  String -> a -> Result (ObjectVal a)
objGet k = fmap getConst . objLens k (pure . Const)

objUpdate :: (ObjectLike a) =>  String -> (ObjectVal a -> Result (ObjectVal a)) -> a -> Result a
objUpdate s f =
  fmap runIdentity .
  objLens s (fmap Identity . f)

objPut :: (ObjectLike a) => String -> Result (ObjectVal a) -> a -> Result a
objPut s = objUpdate s . const

invalidPointer :: Result a
invalidPointer = Error "invalid pointer."

getDynamic ::(Typeable a) => Dynamic -> Result a
getDynamic = maybe (Error "type mismatch") pure . fromDynamic
                 
patchOpArr :: (Typeable (ArrayVal a), Eq (ArrayVal a),
               ToJSON (ArrayVal a), FromJSON (ArrayVal a),
               ToJSON a, FromJSON a,  Eq a, ArrayLike a,
               JsonPatch (ArrayVal a), Typeable a)
           => Operation -> a -> Result a
patchOpArr op arr = case pointerPath $ changePointer op of
  [] -> case op of
    (Add _ val) -> fromJSON val
    (Rem _) -> Error "illegal operation"
    (Rep _ val) -> fromJSON val
    (Cpy _ fromPath) -> 
      getDynamic =<< fst <$> deleteAtArr fromPath arr
    (Mov _ fromPath) -> 
      getDynamic =<< fst <$> deleteAtArr fromPath arr
    (Tst _ val) -> do
      v <- fromJSON val
      when (v /= arr) $
        Error "Test failed"
      return arr
  [key] -> case op of
    (Add _ val) -> do
      v <- fromJSON val
      if isEndKey key
        then pure $ arrAppend arr v
        else do i <- intKey key
                arrInsert i v arr
    (Rem _) -> do
      i <- intKey key
      snd <$> arrDelete i arr
    (Rep _ val) -> do
      i <- intKey key
      arrPut i (fromJSON val) arr
    (Cpy _ (Pointer [fromKey])) -> do
      frm <- intKey fromKey
      v <- arrGet frm arr
      if isEndKey key
        then pure $ arrAppend arr v
        else do to_ <- intKey key
                arrInsert to_ v arr
    (Cpy _ (Pointer (fromKey:fromPath))) -> do
      frm <- intKey fromKey
      v <- arrGet frm arr
      innerDyn <- fst <$> deleteAt (Pointer fromPath) v
      -- don't actually delete anything
      case fromDynamic innerDyn of
        Nothing -> Error "type error"
        Just inner
          | isEndKey key -> pure $ arrAppend arr inner
          | otherwise -> do
              to_ <- intKey key
              arrInsert to_ inner arr
    (Cpy _ _) -> invalidPointer
    (Mov _ (Pointer [fromKey])) -> do
      frm <- intKey fromKey
      (v, arr2) <- arrDelete frm arr
      if isEndKey key
        then pure $ arrAppend arr v
        else do to_ <- intKey key
                arrInsert to_ v arr2
    (Mov _ (Pointer (fromKey:fromPath))) -> do
      frm <- intKey fromKey
      v <- arrGet frm arr
      (innerDyn, v') <- deleteAt (Pointer fromPath) v
      arr' <- arrPut frm (pure v') arr
      case fromDynamic innerDyn of
        Nothing -> Error "type mismatch"
        Just inner
          | isEndKey key -> pure $ arrAppend arr' inner
          | otherwise -> do
              to_ <- intKey key
              arrInsert to_ inner arr'
    (Mov _ _) -> invalidPointer
    (Tst _ val) -> do
      v <- fromJSON val
      i <- intKey key
      v2 <- arrGet i arr
      when (v /= v2) $
        Error "Test failed"
      return arr
  (key:nextPath) -> case op of
    (Cpy _ (Pointer (fromKey:fromPath)))
      | fromKey == key -> do
          i <- intKey key 
          arrUpdate i (patchOp (Cpy (Pointer nextPath)
                                (Pointer fromPath))) arr
    (Cpy _ fromPath) -> do
      innerDyn <- fst <$> deleteAtArr fromPath arr
      addAtArr (changePointer op) innerDyn arr
      
    (Mov _ (Pointer (fromKey:fromPath)))
      | fromKey == key -> do
          i <- intKey key 
          arrUpdate i (patchOp (Mov (Pointer nextPath)
                                (Pointer fromPath))) arr
    (Mov _ (Pointer [])) -> Error "cannot move to child."
    (Mov _ fromPath) -> do
      (innerDyn, arr') <- deleteAtArr fromPath arr
      addAtArr (changePointer op) innerDyn arr'

    (Tst _ val) -> do
      i <- intKey key
      _ <- patchOp (Tst (Pointer nextPath) val) =<< arrGet i arr
      pure arr
    _ -> do
      i <- intKey key
      arrUpdate i (patchOp op{changePointer = Pointer nextPath}) arr
            

valueAtArr :: (JsonPatch (ArrayVal a), ToJSON (ArrayVal a), ArrayLike a) => Pointer -> a -> Result Value
valueAtArr (Pointer [AKey i]) arr = toJSON <$> arrGet i arr
valueAtArr (Pointer (AKey i:keys)) arr = do
  v <- arrGet i arr
  valueAt (Pointer keys) v
valueAtArr _ _ = Error "invalid pointer"

encodeAtArr :: (JsonPatch (ArrayVal a), ToJSON (ArrayVal a), ArrayLike a)
            => Pointer -> a -> Result ByteString
encodeAtArr (Pointer [AKey i]) arr = encode <$> arrGet i arr
encodeAtArr (Pointer (AKey i:keys)) arr = do
  v <- arrGet i arr
  encodeAt (Pointer keys) v
encodeAtArr _ _ = Error "invalid pointer"

deleteAtArr :: (JsonPatch (ArrayVal a), ToJSON (ArrayVal a), ArrayLike a, Typeable (ArrayVal a))
            => Pointer -> a -> Result (Dynamic, a)
deleteAtArr (Pointer [AKey i]) arr = do
  (v, arr2) <- arrDelete i arr
  return (toDyn v, arr2)
deleteAtArr (Pointer (AKey i:keys)) arr = 
  arrLens i (deleteAt (Pointer keys)) arr
deleteAtArr _ _ =
  Error "Invalid pointer"

addAtArr :: (Typeable a, JsonPatch (ArrayVal a), ToJSON (ArrayVal a),
             ArrayLike a, Typeable (ArrayVal a))
            => Pointer -> Dynamic -> a -> Result a
addAtArr (Pointer []) dyn _ = getDynamic dyn
addAtArr (Pointer [OKey "-"]) dyn arr = 
  arrAppend arr <$> getDynamic dyn
addAtArr (Pointer [AKey i]) dyn arr = do
  v <- getDynamic dyn
  arrInsert i v arr
addAtArr (Pointer (AKey i:keys)) dyn arr = 
  arrUpdate i (addAt (Pointer keys) dyn) arr
addAtArr _ _ _ =
  Error "Invalid pointer"

splitList :: Int -> [a] -> Maybe ([a], [a])
splitList i _ | i < 0 = Nothing
splitList 0 xs = Just ([], xs)
splitList _ [] = Nothing
splitList n (x:xs) = do
  (l, r) <- splitList (n-1) xs
  pure (x:l, r)

instance ArrayLike [a] where
  type ArrayVal [a] = a
  arrAppend a v = a ++ [v]
  arrInsert i v lst =
    case splitList i lst of
      Just (l, r) -> pure $ l ++ v:r
      Nothing -> Error "Index out of bounds"
  arrDelete i lst =
    case splitList i lst of
      Just (l, r1:rs) -> pure (r1, l ++ rs)
      _ -> Error "Index out of bounds"
  arrLens i f lst =
    case splitList i lst of
      Just (l, r1:rs) ->
        fmap (\v -> l ++ v:rs) <$> f r1
      _ -> Error "Index out of bounds"

-- declareArrayLikePatch ''[a]

instance (Typeable a, ToJSON a, FromJSON a, Eq a, JsonPatch a) =>
         JsonPatch [a] where
  patchOp = patchOpArr
  valueAt = valueAtArr
  encodeAt = encodeAtArr
  deleteAt = deleteAtArr
  addAt = addAtArr

