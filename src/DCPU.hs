{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}


module DCPU ( DCPU (..)
            , DCPU16
            , Register (..)
            , newDCPU16
            , loadCPU
            , storeCPU
            ) where

import Lens.Micro.TH (makeLenses)
import Lens.Micro
import Lens.Micro.Extras (view)

import GHC.Word (Word16)


data DCPU a = DCPU { _regA  :: a
                   , _regB  :: a
                   , _regC  :: a
                   , _regX  :: a
                   , _regY  :: a
                   , _regZ  :: a
                   , _regI  :: a
                   , _regJ  :: a
                   , _regPC :: a
                   , _regSP :: a
                   , _regO  :: a
                   } deriving (Eq, Show)

$(makeLenses ''DCPU)

type DCPU16 = DCPU Word16


newDCPU16 :: DCPU16
newDCPU16 = DCPU { _regA  = 0
                 , _regB  = 0
                 , _regC  = 0
                 , _regX  = 0
                 , _regY  = 0
                 , _regZ  = 0
                 , _regI  = 0
                 , _regJ  = 0
                 , _regPC = 0
                 , _regSP = 0xFFFF
                 , _regO  = 0
                 }

data Register = A | B | C | X | Y | Z | I | J | PC | SP | O
  deriving (Show, Eq, Enum, Bounded)


loadCPU :: Register -> DCPU a -> a
loadCPU reg = view $ regLens reg

storeCPU :: Register -> a -> DCPU a -> DCPU a
storeCPU reg = set $ regLens reg

regLens :: Register -> Lens' (DCPU a) a
regLens A  = regA
regLens B  = regB
regLens C  = regC
regLens X  = regX
regLens Y  = regY
regLens Z  = regZ
regLens I  = regI
regLens J  = regJ
regLens PC = regPC
regLens SP = regSP
regLens O  = regO
