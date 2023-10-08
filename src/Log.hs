module Log ( registers
           , ram
           ) where

import Data.List (intercalate)
import Control.Monad (forM)

import LSMachine
import Computer

registers :: LSMachine m => m String
registers = do
  regs <- forM [minBound .. maxBound] $ \name -> do
    val <- load (Reg name)
    return (name, val)
  return $ intercalate ", " $
    [show name <> ": " <> show val | (name, val) <- regs]

ram :: LSMachine m => m String
ram = unlines <$> mapM line [(x * 8, x * 8 + 7) | x <- [0 .. fromIntegral $ (memorySize - 1) `div` 8]]
  where
    line (lo, up) = do
        vs <- mapM (load . Ram) [lo .. up]
        return $ show lo ++ ": " ++ unwords (show <$> vs)
