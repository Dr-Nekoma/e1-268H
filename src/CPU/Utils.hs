--{-# LANGUAGE OverloadedStrings #-}

module CPU.Utils ( showWord16
                 ) where

import Data.Word
import Data.Char (intToDigit)
import Data.Bits (shiftR, (.&.))
import Numeric


-- showWord16 :: Word16 -> String
-- showWord16 word = "0x" <> hex
--   where
--     hex = mconcat $ (\n -> showIntAtBase 16 intToDigit n "") . (\n -> (word `shiftR` (4*n)) .&. 0x000f) <$> [3,2,1,0]

showWord16 :: Word16 -> String
showWord16 word = "0x" <> hex
  where
    hex = do
      i <- [3,2,1,0]
      let k = word `shiftR` (4*i) .&. 0x000f
      showHex k ""
