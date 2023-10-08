module Memory (module Memory) where

import GHC.Word (Word16 (..))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Control.Monad.ST

newtype Memory s = Memory (V.MVector s Word16)

newMemory :: Int -> ST s (Memory s)
newMemory size = do
  Memory <$> M.replicate size 0
--  return $ Memory memory

--loadMemory :: Word16 -> Memory s -> ST s Word16
loadMemory :: Word16 -> Memory s -> ST s Word16
loadMemory address (Memory mem) = M.read mem . fromIntegral $ address

--storeMemory :: Word16 -> Word16 -> Memory s -> ST s ()
storeMemory :: Word16 -> Word16 -> Memory s -> ST s (Memory s)
storeMemory address dat m@(Memory mem) = M.write mem (fromIntegral address) dat >> return m


loadMemory' :: Word16 -> ST s (Memory s) -> ST s Word16
loadMemory' address mem = loadMemory address =<< mem

--storeMemory' :: Word16 -> Word16 -> ST s (Memory s) -> ST s ()
storeMemory' :: Word16 -> Word16 -> ST s (Memory s) -> ST s (Memory s)
storeMemory' address dat mem = storeMemory address dat =<< mem
