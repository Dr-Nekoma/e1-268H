module Lib
    ( someFunc
    , module DCPU
    , module Memory
    , module LSMachine
    , module Computer
    , module Log
    , module Utils
    ) where

import DCPU
import Memory
import LSMachine
import Computer
import Log
import Utils

someFunc :: IO ()
someFunc = putStrLn "someFunc"
