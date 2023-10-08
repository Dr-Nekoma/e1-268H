{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE LiberalTypeSynonyms #-}


module Computer ( Computer
--                , IOComputer
--                , runIOComputer
--                , STComputer
--                , runSTComputer
                , loadProgram
                , memorySize
               -- , newComputer
--                , initialize
                , newComputer'
                , runIOComputer'
                , test
                ) where

import Data.Word (Word16)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.ST (ST, RealWorld, stToIO, runST)
import Control.Monad.Trans (lift, MonadIO)
import Control.Monad.IO.Class (liftIO)

import Debug.Trace

import DCPU
import Memory
import LSMachine

data Computer s = Computer { dcpu :: DCPU Word16
                           , dmemory :: Memory s
                           }

data Computer' s = Computer' { dcpu' :: DCPU Word16
                             , dmemory' :: ST s (Memory s) }

memorySize :: Int
memorySize = 128


-- newtype IOComputer' a = IOComputer' (StateT (Computer' RealWorld) IO a)
--   deriving (Functor, Applicative, Monad, MonadIO)

type IOComputer' = StateT (Computer' RealWorld) IO

--instance LSMachine (StateT (Computer' RealWorld) IO)  where
instance LSMachine IOComputer' where
  load :: Address -> IOComputer' Word16
  load address = do
    computer <- get
    case address of
      Reg r -> return $ loadCPU r (dcpu' computer)
      Ram word -> lift . stToIO . loadMemory' word $ dmemory' computer

  store :: Address -> Word16 -> IOComputer' ()
  store address word = do
    computer <- get
    case address of
      Reg r -> put computer { dcpu' = storeCPU r word (dcpu' computer) }
      Ram addr -> put computer { dmemory' = storeMemory' addr word $ dmemory' computer }


        -- storeMemory' addr word $ dmemory' computer
--         put computer { dmemory' = storeMemory' addr word $ dmemory' computer }
--         --return $ runST $ storeMemory' addr word $ dmemory' computer
-- --        lift . stToIO . storeMemory' addr word $ dmemory' computer


newComputer' :: Computer' RealWorld
newComputer' = Computer' newDCPU16 (newMemory memorySize)


runIOComputer' :: IOComputer' a -> Computer' RealWorld -> IO (a, Computer' RealWorld)
--runIOComputer' (IOComputer' prog) = runStateT prog
runIOComputer' = runStateT

test = runIOComputer' (loadProgram 10) newComputer'


loadProgram :: LSMachine m => Word16 -> m ()
loadProgram = store (Ram 0x0000)
-- loadProgram :: LSMachine m => Word16 -> m ()
-- loadProgram = store (Ram 0x0000)


-- IO Computer --

-- newtype IOComputer a = IOComputer (StateT (Computer RealWorld) IO a)
--   deriving (Functor, Applicative, Monad, MonadIO)

-- type IOComputer a = (StateT (Computer RealWorld) IO a)

-- --instance LSMachine IOComputer where
-- instance LSMachine (StateT (Computer RealWorld) IO) where
--   load address = do
--     computer <- get
--     case address of
--       Reg r -> return $ loadCPU (dcpu computer) r
--       Ram word -> lift $ stToIO $ loadMemory word $ dmemory computer

--   store address word = do
--     computer <- get
--     case address of
--       Reg r -> put computer { dcpu = storeCPU (dcpu computer) r word }
--       Ram addr -> lift $ stToIO $ storeMemory addr word $ dmemory computer

-- -- runIOComputer :: IOComputer a -> IO (a, Computer RealWorld)
-- -- runIOComputer (IOComputer state) = do
-- --   mem <- stToIO $ newMemory memorySize
-- --   runStateT state $ Computer newCpu mem

-- --runIOComputer :: Computer a -> IO (a, Computer a)

-- --newComputer :: IOComputer a
-- --initialize :: StateT (Computer RealWorld) IO ()
-- initialize :: IOComputer ()
-- initialize = do
--   mem <- liftIO $ stToIO $ newMemory memorySize
--   put $ Computer newCpu mem
--   return ()



-- -- initialize :: StateT (Computer s) IO ()


-- -- newIOComputer :: IOComputer a
-- -- newIOComputer = StateT $ \s ->
-- --   mem <- stToIO $ newMemory 0x10000





-- -- ST Computer --

-- newtype STComputer s a = STComputer (StateT (Computer s) (ST s) a)
--   deriving (Functor, Applicative, Monad)

-- instance LSMachine (STComputer s) where
--   load address = STComputer $ do
--     computer <- get
--     case address of
--       Reg r -> return $ loadCPU (dcpu computer) r
--       Ram word -> lift $ loadMemory word $ dmemory computer

--   store address word = STComputer $ do
--     computer <- get
--     case address of
--       Reg r -> put computer { dcpu = storeCPU (dcpu computer) r word }
--       Ram addr -> lift $ storeMemory addr word $ dmemory computer

-- runSTComputer :: STComputer s a -> ST s (a, Computer s)
-- runSTComputer (STComputer state) = do
--   mem <- newMemory 0x10000
--   runStateT state $ Computer newCpu mem
