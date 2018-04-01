{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, MultiWayIf,
   ExistentialQuantification #-}
module Data.Aeson.Diff.Generic
  (Data.Aeson.Diff.Generic.patch, applyOperation, JsonPatch(..),
   getDynamic, getValueAtPointer, getDataAtPointer, GetSet(..), FieldLens(..)
  ) where

import Data.Aeson.Types
import Data.Aeson.Patch
import qualified Data.Aeson.Diff as Diff
import Data.Aeson.Pointer as Pointer
import Control.Monad
import qualified Data.Text as T
import Data.Aeson.Diff.Generic.Types
import Data.Aeson.Diff.Generic.Instances()

-- | Apply the json patch to the data.
patch :: JsonPatch a => Patch -> a -> Result a
patch = foldr (>=>) pure . map applyOperation . patchOperations
{-# NOINLINE patch #-}
{-# RULES "patch/Value" patch = Diff.patch #-} 

-- | Apply a single operation to the data.
applyOperation :: JsonPatch a => Operation -> a -> Result a
applyOperation op v = case patchOp op v of
  Error msg -> Error $ opError op ++ msg
  Success ret -> Success ret
{-# INLINE [1] applyOperation #-}
{-# RULES "applyOperation/Value" applyOperation = Diff.applyOperation #-} 

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

patchOp :: JsonPatch a => Operation -> a -> Result a
patchOp (Add ptr val) s = addAtPointer ptr s val fromJSON
patchOp (Rem ptr) s = snd <$> deleteAtPointer ptr s (const ())
patchOp (Cpy toPath fromPath) s = copyPath fromPath toPath s
patchOp (Mov toPath fromPath) s = movePath fromPath toPath s
patchOp (Rep ptr val) s = replaceAtPointer ptr s val fromJSON
patchOp (Tst ptr val) s = testAtPointer ptr s val fromJSON
