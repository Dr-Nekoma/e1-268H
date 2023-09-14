module Memory (module Memory) where

import GHC.Word (Word16 (..))

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M

import Control.Monad.ST

data Memory s = Memory (V.MVector s Word16)

newMemory :: Int -> ST s (Memory s)
newMemory size = do
  Memory <$> M.replicate size 0
--  return $ Memory memory

loadMemory :: Memory s -> Int -> ST s Word16
loadMemory (Memory mem) address = do
  M.read mem address

storeMemory :: Memory s -> Int -> Word16 -> ST s ()
storeMemory (Memory mem) address dat = do
  M.write mem address dat
