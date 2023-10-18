module Lib
    ( someFunc
    , module DCPU
    , module Memory
    , module LSMachine
    , module Computer
    , module Log
    , module CPU.Utils
    ) where

import DCPU
import Memory
import LSMachine
import Computer
import Log
import CPU.Utils

someFunc :: IO ()
someFunc = putStrLn "someFunc"
