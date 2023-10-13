{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Computer
  -- ( Computer
  --               , memorySize
  --               , newIOComputer
  --               , runIOComputer
  --               , newSTComputer
  --               , runSTComputer
  --               )
where

import Data.Word (Word16)
import Data.Bits (shiftL)

import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.ST (ST, RealWorld, stToIO)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B

import DCPU
import Instruction
import Memory
import LSMachine


data Computer s = Computer { _dcpu :: DCPU Word16
                           , _dmemory :: ST s (Memory s) }

memorySize :: Int
memorySize = 128



--- Computer operations ---

fetch :: (LSMachine m) => m Word16
fetch = do
  pc <- load (Reg PC)
  store (Reg PC) $ pc + 1
  load (Ram pc)

decode :: (LSMachine m) => m (Instruction Operand)
decode = do
  instr <- fetch
  return . decodeInstruction $ instr

-- exec :: (LSMachine m) => Instruction Operand -> m ()
-- exec (BasicInstruction SET a b) = do


-- ignores the last byte in case there is an odd number of input bytes
loadProgram :: (LSMachine m) => B.ByteString -> m ()
loadProgram bs = loop 0
  where
    len = B.length bs
    loop !i
      | i + 1 >= len = return ()
      | otherwise = do
          -- read two bytes as the memory word is 16 bit wide
          let !byte1 = fromIntegral $ B.index bs i
              !byte2 = fromIntegral $ B.index bs (i + 1)
              !word  = (byte1 `shiftL` 8) + byte2
              !addr  = fromIntegral $ i `div` 2
          store (Ram addr) word
          loop $ i + 2



--- IO Computer ---

type IOComputer = StateT (Computer RealWorld) IO

instance LSMachine IOComputer where
  load :: Address -> IOComputer Word16
  load address = do
    computer <- get
    case address of
      Reg r -> return $ loadCPU r (_dcpu computer)
      Ram word -> lift . stToIO . loadMemory word $ _dmemory computer

  store :: Address -> Word16 -> IOComputer ()
  store address word = do
    computer <- get
    case address of
      Reg r -> put computer { _dcpu = storeCPU r word (_dcpu computer) }
      Ram addr -> put computer { _dmemory = storeMemory addr word $ _dmemory computer }


newIOComputer :: Computer RealWorld
newIOComputer = Computer newDCPU16 (newMemory memorySize)

runIOComputer :: IOComputer a -> Computer RealWorld -> IO (a, Computer RealWorld)
runIOComputer = runStateT

--- ST Computer ---

type STComputer s = StateT (Computer s) (ST s)

instance LSMachine (STComputer s) where
  load :: Address -> STComputer s Word16
  load address = do
    computer <- get
    case address of
      Reg r -> return $ loadCPU r (_dcpu computer)
      Ram word -> lift . loadMemory word $ _dmemory computer

  store :: Address -> Word16 -> STComputer s ()
  store address word = do
    computer <- get
    case address of
      Reg r -> put computer { _dcpu = storeCPU r word (_dcpu computer) }
      Ram addr -> put computer { _dmemory = storeMemory addr word $ _dmemory computer }


newSTComputer :: Computer s
newSTComputer = Computer newDCPU16 (newMemory memorySize)

runSTComputer :: STComputer s a -> Computer s -> ST s (a, Computer s)
runSTComputer = runStateT
