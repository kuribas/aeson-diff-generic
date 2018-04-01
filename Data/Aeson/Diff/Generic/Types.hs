{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, DefaultSignatures,
    MultiWayIf, ExistentialQuantification, TemplateHaskell #-}
module Data.Aeson.Diff.Generic.Types where

import Data.Aeson.Types
import Data.Aeson.Pointer as Pointer
import Control.Monad
import Data.Dynamic

data GetSet s = forall v. JsonPatch v => GetSet v (v -> Result s)

class (Eq s, ToJSON s, FromJSON s, Typeable s) => FieldLens s where
  fieldLens :: Key -> s -> Result (GetSet s)
  fieldLens _ _ = Error "Invalid pointer"

  deleteAt :: Key -> s -> (forall v.(JsonPatch v) => v -> r)
           -> Result (r, s)
  deleteAt _ _ _ = Error "Illegal operation"

  insertAt :: Key -> s -> r -> (forall v.(JsonPatch v) => r -> Result v)
           -> Result s
  insertAt _ _ _ _ = Error "Illegal operation"

class (Eq s, ToJSON s, FromJSON s, Typeable s) => JsonPatch s where
  getAtPointer :: Pointer -> s -> (forall v.JsonPatch v => v -> r) -> Result r
  default getAtPointer :: FieldLens s => Pointer -> s
                       -> (forall v.JsonPatch v => v -> r) -> Result r
  getAtPointer (Pointer []) s f = pure $ f s
  getAtPointer (Pointer (key:path)) s f = do
    GetSet s2 _ <- fieldLens key s
    getAtPointer (Pointer path) s2 f

  deleteAtPointer :: Pointer -> s -> (forall v.JsonPatch v => v -> r)
                  -> Result (r, s)
  default deleteAtPointer :: FieldLens s => Pointer -> s
                          -> (forall v.JsonPatch v => v -> r) -> Result (r, s)
  deleteAtPointer (Pointer []) _ _ = Error "Invalid pointer"
  deleteAtPointer (Pointer [key]) s f = deleteAt key s f
  deleteAtPointer (Pointer (key:path)) s f = do
    GetSet v setr <- fieldLens key s
    join $ traverse setr <$> deleteAtPointer (Pointer path) v f

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

  copyPath :: Pointer -> Pointer -> s -> Result s
  default copyPath :: FieldLens s => Pointer -> Pointer -> s -> Result s
  copyPath (Pointer (fromKey: fromPath)) (Pointer (toKey: toPath)) s
    | fromKey == toKey =
      updateAtKey toKey s $
      copyPath (Pointer fromPath) (Pointer toPath)
  copyPath from to_ s = do
    v <- getAtPointer from s toDyn
    addAtPointer to_ s v getDynamic

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

getValueAtPointer :: JsonPatch s => Pointer -> s -> Result Value
getValueAtPointer p s = getAtPointer p s toJSON
{-# INLINE [1] getValueAtPointer #-}
{-# RULES "getValueAtPointer/Value" getValueAtPointer = Pointer.get #-} 

getDataAtPointer :: (JsonPatch s, Typeable a) => Pointer -> s
                 -> Result a
getDataAtPointer p s = getDynamic =<< getAtPointer p s toDyn
{-# INLINE [1] getDataAtPointer #-}  
{-# RULES "getDataAtPointer/Value" getDataAtPointer = Pointer.get #-} 

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

