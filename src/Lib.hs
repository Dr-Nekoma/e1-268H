module Lib
    ( someFunc
    , module DCPU
    , module Memory
    , module LSMachine
    , module Computer
    , module Log
    ) where

import DCPU
import Memory
import LSMachine
import Computer
import Log

someFunc :: IO ()
someFunc = putStrLn "someFunc"
