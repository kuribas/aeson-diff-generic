{-# LANGUAGE OverloadedStrings, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Data.Aeson.Diff.Generic where
import Data.Aeson
import Data.Aeson.Patch
import Data.Aeson.Diff
import Data.Aeson.Pointer
import Control.Monad 
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.ByteString.Lazy (ByteString)
import Data.Dynamic

import Data.Functor.Const
import Data.Functor.Compose

type FieldLens s v = forall f.Functor f => (v -> f v) -> f s
type LensConsumer s r = forall v. (JsonPatch v) => FieldLens s v -> Result r

class (Eq s, ToJSON s, FromJSON s, Typeable s) => JsonPatch s where
  fieldLens :: s -> Key -> LensConsumer s r -> Result r
  deleteAt :: Key -> s -> (forall v.(JsonPatch v) => v -> r)
           -> Result (r, s)
  insertAt :: Key -> s -> r -> (forall v.(JsonPatch v) => r -> Result v)
           -> Result s

setAtKey :: JsonPatch s => Key -> s -> r -> (forall v.JsonPatch v => r -> Result v) -> Result s
setAtKey k s a f = fieldLens s k $ \l -> l $ const $ f a

updateAtKey :: JsonPatch s => Key -> s -> (forall v.JsonPatch v => v -> Result v) -> Result s
updateAtKey k s f = fieldLens s k (\l -> l f)
  
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

getDynamic ::(Typeable a) => Dynamic -> Result a
getDynamic = maybe (Error "type mismatch") pure . fromDynamic

-- (Result . Const r) v

getAtPointer :: JsonPatch s => Pointer -> s -> (forall v.JsonPatch v => v -> r) -> Result r
getAtPointer (Pointer []) s f = pure $ f s
getAtPointer (Pointer (key:path)) s f =
  fieldLens s key $ \l ->
    fmap getConst $ getCompose $
    l (\v -> Compose $ Const <$> getAtPointer (Pointer path) v f)
    
getDynAtPointer :: JsonPatch s => Pointer -> s -> Result Dynamic
getDynAtPointer p s = getAtPointer p s toDyn

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
  v <- getAtPointer from s toDyn
  addAtPointer to_ s v getDynamic

movePath :: JsonPatch s => Pointer -> Pointer -> s -> Result s
movePath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
  | fromKey == toKey =
    updateAtKey toKey s $
    movePath (Pointer fromPath) (Pointer toPath)
movePath (Pointer []) (Pointer []) s = pure s
movePath (Pointer []) _ _ = Error "cannot move to child"
movePath from to_ s = do
  (v, s') <- deleteAtPointer from s toDyn
  addAtPointer to_ s' v getDynamic

replaceAtPath :: JsonPatch s => Pointer -> s -> r ->
             (forall v.JsonPatch v => r -> Result v) -> Result s
replaceAtPath (Pointer []) _ v f = f v
replaceAtPath (Pointer [key]) s val f =
  setAtKey key s val f
replaceAtPath (Pointer (key:path)) s val f =
  updateAtKey key s (\v -> replaceAtPath (Pointer path) v val f)

testAtPath :: JsonPatch s => Pointer -> s -> r ->
              (forall v.JsonPatch v => r -> Result v) -> Result s
testAtPath (Pointer []) s r f = do
  v <- f r
  if v == s then pure s
    else Error "Test failed"
testAtPath (Pointer (key:path)) s val f = do
  _ <- updateAtKey key s $ \v ->
    testAtPath (Pointer path) v val f
  pure s

patchOp :: JsonPatch a => Operation -> a -> Result a
patchOp (Add ptr val) s = addAtPointer ptr s val fromJSON
patchOp (Rem ptr) s = snd <$> deleteAtPointer ptr s (const ())
patchOp (Cpy toPath fromPath) s = copyPath fromPath toPath s
patchOp (Mov toPath fromPath) s = movePath fromPath toPath s
patchOp (Rep ptr val) s = replaceAtPath ptr s val fromJSON
patchOp (Tst ptr val) s = testAtPath ptr s val fromJSON
            
intKey :: Key -> Result Int
intKey (OKey _) = Error "expected Array Key."
intKey (AKey i) = pure i

strKey :: Key -> String
strKey (OKey s) = T.unpack s
strKey (AKey i) = show i

isEndKey :: Key -> Bool
isEndKey = (== OKey "-")

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
