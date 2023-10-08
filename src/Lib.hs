module Lib
    ( someFunc
    , module DCPU16
    , module Memory
    , module LSMachine
    , module Computer
    ) where

import DCPU16
import Memory
import LSMachine
import Computer

someFunc :: IO ()
someFunc = putStrLn "someFunc"
