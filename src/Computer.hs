{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Computer ( Computer
                , memorySize
                , newIOComputer
                , runIOComputer
                , newSTComputer
                , runSTComputer
                ) where

import Data.Word (Word16)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.ST (ST, RealWorld, stToIO)
import Control.Monad.Trans (lift)

import DCPU
import Memory
import LSMachine


data Computer s = Computer { _dcpu :: DCPU Word16
                           , _dmemory :: ST s (Memory s) }

memorySize :: Int
memorySize = 128


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
