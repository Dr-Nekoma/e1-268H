{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Computer ( Computer
                , memorySize
                , newIOComputer
                , runIOComputer
                , newSTComputer
                , runSTComputer
                )
where

import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)

import Control.Monad (unless, void)
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

decode :: (LSMachine m) =>  Word16 -> m (Instruction Value)
decode = loadOperands . decodeInstruction

execute :: (LSMachine m) => Instruction Value -> m ()
-- SET a, b - sets a to b
execute (BasicInstruction SET a b) = loadValue b >>= storeValue a
-- ADD a, b - sets a to a+b, sets O to 0x0001 if there's an overflow, 0x0 otherwise
execute (BasicInstruction ADD a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a (x + y) -- 16 bit (+)
  -- overflow if x + y exceeds 16 bits
  let (x', y') = (fromIntegral x, fromIntegral y)
      overflow = x' + y' > (0xffff :: Int)
  store (Reg O) $ if overflow then 0x0001 else 0x0000
-- SUB a, b - sets a to a-b, sets O to 0xffff if there's an underflow, 0x0 otherwise
execute (BasicInstruction SUB a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a (x - y)
  -- underflow if x - y goes negative
  let (x', y') = (fromIntegral x, fromIntegral y)
      underflow = x' - y' < (0x0000 :: Int)
  store (Reg O) $ if underflow then 0xffff else 0x0000
-- MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&0xffff
execute (BasicInstruction MUL a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a (x * y)
  let (x', y') = (fromIntegral x, fromIntegral y)
      -- extend precision
      overflow = ((x' * y') `shiftR` 16) .&. 0xffff :: Word
  -- then truncate
  store (Reg O) $ fromIntegral overflow
-- DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff. if b==0, sets a and O to 0 instead.
execute (BasicInstruction DIV a b) = do
  x <- loadValue a
  y <- loadValue b
  if y == 0
    then do
      storeValue a 0x0000
      store (Reg O) 0x0000
    else do
      storeValue a (x `div` y)
      let (x', y') = (fromIntegral x, fromIntegral y)
          -- extend precision
          overflow = ((x' `shiftL` 16) `div` y') .&. 0xffff :: Word
      -- then truncate
      store (Reg O) $ fromIntegral overflow
-- MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
execute (BasicInstruction MOD a b) = do
  x <- loadValue a
  y <- loadValue b
  if y == 0
    then storeValue a 0x0000
    else storeValue a (x `mod` y)
-- SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&0xffff
execute (BasicInstruction SHL a b) = do
  x <- loadValue a
  y <- loadValue b
  let (x', y') = (fromIntegral x, fromIntegral y)
      -- extend precision
      overflow = ((x' `shiftL` y') `shiftR` 16) .&. 0xfff :: Word
  storeValue a $ x `shiftL` y'
  -- then truncate
  store (Reg O) $ fromIntegral overflow
-- SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
execute (BasicInstruction SHR a b) = do
  x <- loadValue a
  y <- loadValue b
  let (x', y') = (fromIntegral x, fromIntegral y)
      -- extend precision
      overflow = ((x' `shiftL` 16) `shiftR` y') .&. 0xfff :: Word
  storeValue a $ x `shiftR` y'
  -- then truncate
  store (Reg O) $ fromIntegral overflow
-- AND a, b - sets a to a&b
execute (BasicInstruction AND a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a $ x .&. y
-- BOR a, b - sets a to a|b
execute (BasicInstruction BOR a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a $ x .|. y
-- XOR a, b - sets a to a^b
execute (BasicInstruction XOR a b) = do
  x <- loadValue a
  y <- loadValue b
  storeValue a $ x `xor` y
-- IFE a, b - performs next instruction only if a==b
execute (BasicInstruction IFE a b) = do
  x <- loadValue a
  y <- loadValue b
  unless (x == y) $ void fetch
-- IFN a, b - performs next instruction only if a!=b
execute (BasicInstruction IFN a b) = do
  x <- loadValue a
  y <- loadValue b
  unless (x /= y) $ void fetch
-- IFG a, b - performs next instruction only if a>b
execute (BasicInstruction IFG a b) = do
  x <- loadValue a
  y <- loadValue b
  unless (x > y) $ void fetch
-- IFB a, b - performs next instruction only if (a&b)!=0
execute (BasicInstruction IFB a b) = do
  x <- loadValue a
  y <- loadValue b
  unless (x .&. y /= 0) $ void fetch
-- JSR a - pushes the address of the next instruction to the stack, then sets PC to a
execute (NonBasicInstruction JSR a) = do
  pc <- load (Reg PC)
  addr <- loadOperand OpPush
  execute (BasicInstruction SET addr (Literal pc))
  x <- loadValue a
  store (Reg PC) x
-- Reserved instruction space: fails silently
execute _ = return ()


data Value = Literal Word16
           | Address Address
           deriving Show

loadValue :: (LSMachine m) => Value -> m Word16
loadValue (Literal word) = return word
loadValue (Address addr) = load addr

storeValue :: (LSMachine m) => Value -> Word16 -> m ()
-- * If any instruction tries to assign a literal value, the assignment fails silently.
storeValue (Literal _) _ = return ()
--Other than that, the instruction behaves as normal.
storeValue (Address addr) word = store addr word

loadOperands :: (LSMachine m) => Instruction Operand -> m (Instruction Value)
loadOperands (BasicInstruction op a b) = do
  aa <- loadOperand a
  bb <- loadOperand b
  return $ BasicInstruction op aa bb
loadOperands (NonBasicInstruction op a) = do
  aa <- loadOperand a
  return $ NonBasicInstruction op aa
loadOperands (UnknownInstruction w) = return $ UnknownInstruction w

loadOperand :: (LSMachine m) => Operand -> m Value
loadOperand (OpRegister reg) = return . Address . Reg $ reg
loadOperand (OpRegisterPointer reg) = do
  regContent <- load (Reg reg)
  return . Address . Ram $ regContent
loadOperand (OpNextWordPlusRegisterPointer reg) = do
  next <- fetch
  regContent <- load (Reg reg)
  return . Address . Ram $ next + regContent
loadOperand OpPop = do
  sp <- load (Reg SP)
  store (Reg SP) (sp + 1)
  return . Address . Ram $ sp
loadOperand OpPeek = do
  sp <- load (Reg SP)
  return . Address . Ram $ sp
loadOperand OpPush = do
  spDec <- subtract 1 <$> load (Reg SP)
  store (Reg SP) spDec
  return . Address . Ram $ spDec
loadOperand OpSp = return . Address . Reg $ SP
loadOperand OpPc = return . Address . Reg $ PC
loadOperand OpO = return . Address . Reg $ O
loadOperand OpNextWordPointer = do
  next <- fetch
  return . Address . Ram $ next
loadOperand OpNextWordLiteral = Literal <$> fetch
loadOperand (OpLiteral word) = return $ Literal word

-- exec :: (LSMachine m) => Instruction Operand -> m ()
-- exec (BasicInstruction SET a b) = do

-- loadProgramFromFile :: (LSMachine m) => FilePath -> m ()
-- loadProgramFromFile = loadProgram . B.fromFilePath

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
