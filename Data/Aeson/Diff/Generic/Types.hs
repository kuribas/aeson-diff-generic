{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, DefaultSignatures,
    ExistentialQuantification, TemplateHaskell, TupleSections #-}
module Data.Aeson.Diff.Generic.Types where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Data.Dynamic
import Control.Applicative ((<$>), pure)

-- | An existentially quantified getter and setter.  The data inside
-- is manipulated using json conversion functions (`toJSON`,
-- `fromJSON`), or "Data.Dynamic" (`getDynamic`, `toDyn`, etc...).
data GetSet s = forall v. JsonPatch v => GetSet v (v -> Result s)

class (Eq s, ToJSON s, FromJSON s, Typeable s) => FieldLens s where
  -- | Map a key to a getter and setter on the given data.
  fieldLens :: Key -> s -> Result (GetSet s)
  fieldLens _ _ = Error "Invalid pointer"

  -- | Delete and return the data at the given key.  The helper
  -- function determines which value to return: `toJSON` to return an
  -- aeson `Value`, `toDyn` to return a `Dynamic`.
  deleteAt :: Key -> s -> (forall v.(JsonPatch v) => v -> r)
           -> Result (r, s)
  deleteAt _ _ _ = Error "Illegal operation"

  -- | Insert a value at the given key.  The helper function
  -- determines how to convert the value: `fromJSON` to convert from
  -- an aeson `Value`, `getDynamic` to convert from a `Dynamic`.
  insertAt :: Key -> s -> r -> (forall v.(JsonPatch v) => r -> Result v)
           -> Result s
  insertAt _ _ _ _ = Error "Illegal operation"

-- | This class defines all operations necessary for applying patches.
-- Instances can be written by hand, however it's easier to use the
-- `fieldLens` class instead, or to derive it automatically using the
-- template haskell functions from the "Data.Aeson.Diff.Generic.TH"
-- module. The default implementation is based on `fieldLens`, which
-- matches a string to a existentially quantified getter and setter
-- (the `GetSet` datatype).  The Instances can be found in the
-- "Data.Aeson.Diff.Generic.Instances" module, which this module
-- exports.
class (Eq s, ToJSON s, FromJSON s, Typeable s) => JsonPatch s where
  -- | Retrieve the value at the pointer.  To get back a json `Value`
  -- use `toJSON` as the helper function, to get back a `Dynamic` use
  -- `toDyn`.
  getAtPointer :: Pointer -> s -> (forall v.JsonPatch v => v -> r) -> Result r
  default getAtPointer :: FieldLens s => Pointer -> s
                       -> (forall v.JsonPatch v => v -> r) -> Result r
  getAtPointer (Pointer []) s f = pure $ f s
  getAtPointer (Pointer (key:path)) s f = do
    GetSet s2 _ <- fieldLens key s
    getAtPointer (Pointer path) s2 f

  -- | Remove the value at pointer from the data.  The original
  -- value is returned.  To get back a json `Value` use `toJSON` as
  -- the helper function, to get back a `Dynamic` use `toDyn`.
  deleteAtPointer :: Pointer -> s -> (forall v.JsonPatch v => v -> r)
                  -> Result (r, s)
  default deleteAtPointer :: FieldLens s => Pointer -> s
                          -> (forall v.JsonPatch v => v -> r) -> Result (r, s)
  deleteAtPointer (Pointer []) _ _ = Error "Invalid pointer"
  deleteAtPointer (Pointer [key]) s f = deleteAt key s f
  deleteAtPointer (Pointer (key:path)) s f = do
    GetSet v setr <- fieldLens key s
    (r, s2) <- deleteAtPointer (Pointer path) v f
    (r, ) <$> setr s2

  -- | Add a value at the pointer.  To insert a json `Value`, use
  -- `fromJSON` as the helper function, to insert a `Dynamic` use `getDynamic`.
  addAtPointer :: Pointer -> s -> r ->
               (forall v.JsonPatch v => r -> Result v) -> Result s
  default addAtPointer :: FieldLens s => Pointer -> s -> r
                       -> (forall v.JsonPatch v => r -> Result v) -> Result s
  addAtPointer (Pointer []) _ v f = f v
  addAtPointer (Pointer [key]) s val f =
    insertAt key s val f
  addAtPointer (Pointer (key:path)) s val f =
    updateAtKey key s $ \v ->
    addAtPointer (Pointer path) v val f

  -- | copyPath @src@ @dst@ s.  Copy the value at src to dest in s.
  -- If the types don't match an error is returned
  copyPath :: Pointer -> Pointer -> s -> Result s
  default copyPath :: FieldLens s => Pointer -> Pointer -> s -> Result s
  copyPath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
    | fromKey == toKey =
      updateAtKey toKey s $
      copyPath (Pointer fromPath) (Pointer toPath)
  copyPath from to_ s = do
    v <- getAtPointer from s toDyn
    addAtPointer to_ s v getDynamic

  -- | movePath @src@ @dst@ s.  Move the value from src to dest in
  -- s. If the types don't match an error is returned
  movePath :: Pointer -> Pointer -> s -> Result s
  default movePath :: FieldLens s => Pointer -> Pointer -> s -> Result s
  movePath (Pointer []) (Pointer []) s = pure s
  movePath (Pointer []) _ _ = Error "cannot move to child"
  movePath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
    | fromKey == toKey =
      updateAtKey toKey s $
      movePath (Pointer fromPath) (Pointer toPath)
  movePath from to_ s = do
    (v, s') <- deleteAtPointer from s toDyn
    addAtPointer to_ s' v getDynamic

  -- | Replace the value at the pointer with the new value. To replace
  -- using a json `Value`, use `fromJSON` as the helper function, to
  -- insert a `Dynamic` use `getDynamic`.
  replaceAtPointer :: Pointer -> s -> r ->
                      (forall v.JsonPatch v => r -> Result v) -> Result s
  default replaceAtPointer :: FieldLens s => Pointer -> s -> r
                           -> (forall v.JsonPatch v => r -> Result v)
                           -> Result s
  replaceAtPointer (Pointer []) _ v f = f v
  replaceAtPointer (Pointer [key]) s val f =
    setAtKey key s val f
  replaceAtPointer (Pointer (key:path)) s val f =
    updateAtKey key s (\v -> replaceAtPointer (Pointer path) v val f)
  {-# INLINABLE replaceAtPointer #-}  

  -- | Test if the value at the pointer matches the given value (after
  -- conversion).  Return the data if they match, give an error
  -- otherwise.
  testAtPointer :: Pointer -> s -> r ->
                (forall v.JsonPatch v => r -> Result v) -> Result s
  default testAtPointer :: FieldLens s => Pointer -> s -> r
                        -> (forall v.JsonPatch v => r -> Result v) -> Result s
  testAtPointer (Pointer []) s r f = do
    v <- f r
    if v == s then pure s
      else Error "Test failed"
  testAtPointer (Pointer (key:path)) s val f = do
    _ <- updateAtKey key s $ \v ->
      testAtPointer (Pointer path) v val f
    pure s
  {-# INLINABLE testAtPointer #-}    

-- | Get the json value at the pointer.  Gives an error if the
-- destination doesn't exist.
getValueAtPointer :: JsonPatch s => Pointer -> s -> Result Value
getValueAtPointer p s = getAtPointer p s toJSON
{-# INLINE [1] getValueAtPointer #-}
{-# RULES "getValueAtPointer/Value" getValueAtPointer = Pointer.get #-} 

-- | Get the value at the pointer.  Gives and error If the types don't
-- match or if the destination doesn't exist.
getDataAtPointer :: (JsonPatch s, Typeable a) => Pointer -> s
                 -> Result a
getDataAtPointer p s = getDynamic =<< getAtPointer p s toDyn
{-# INLINE [1] getDataAtPointer #-}  
{-# RULES "getDataAtPointer/Value" getDataAtPointer = Pointer.get #-} 

-- | Read a `Dynamic` value into the `Result` Monad.  Gives a type error if
-- the results don't match.
getDynamic ::(Typeable a) => Dynamic -> Result a
getDynamic = maybe (Error "Couldn't match types") pure . fromDynamic

updateAtKey :: FieldLens s => Key -> s
            -> (forall v.JsonPatch v => v -> Result v)
            -> Result s
updateAtKey key s f = do
  GetSet v setr <- fieldLens key s
  setr =<< f v
{-# INLINE updateAtKey #-}

setAtKey :: FieldLens s => Key -> s -> r
         -> (forall v.JsonPatch v => r -> Result v)
         -> Result s
setAtKey k s a f = updateAtKey k s $ const (f a)
{-# INLINE setAtKey #-}

