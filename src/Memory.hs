module Memory (module Memory) where

import GHC.Word (Word16 (..))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Control.Monad.ST

newtype Memory s = Memory { memoryBuffer :: V.MVector s Word16 }

newMemory :: Int -> ST s (Memory s)
newMemory size = Memory <$> M.replicate size 0

loadMemory :: Word16 -> ST s (Memory s) -> ST s Word16
loadMemory address memory = do
  buf <- memoryBuffer <$> memory
  M.read buf $ fromIntegral address

storeMemory :: Word16 -> Word16 -> ST s (Memory s) -> ST s (Memory s)
storeMemory address dat memory = do
  buf <- memoryBuffer <$> memory
  M.write buf (fromIntegral address) dat
  return $ Memory buf
