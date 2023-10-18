module Log ( registers
           , ram
           , dumpMachine
           ) where

import Data.List (intersperse)

import DCPU (Register(..))
import LSMachine
import Computer
import CPU.Utils

registers :: LSMachine m => m String
registers = do
  dump [[PC, A, X, I], [SP, B, Y, J], [O, C, Z]]
  where
    showReg reg = do
      regValue <- load $ Reg reg
      return $ mconcat [show reg, ": ", showWord16 regValue]

    line regs = mconcat . intersperse "\t" <$> traverse showReg regs
    dump rows = mconcat . intersperse "\n" <$> traverse line rows

ram :: LSMachine m => m String
ram = unlines <$> mapM line [(x * 8, x * 8 + 7) | x <- [0 .. fromIntegral $ (memorySize - 1) `div` 8]]
  where
    line (lo, up) = do
        vs <- mapM (load . Ram) [lo .. up]
        return $ showWord16 lo ++ ":\t " ++ unwords (showWord16 <$> vs)

dumpMachine :: LSMachine m => m String
dumpMachine = do
  regs <- registers
  memo <- ram
  return $ "---\n" <> regs <> "\n" <> memo <> "---\n"
