{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module DCPU ( DCPU (..)
            , newDCPU16
            , loadCPU
            , storeCPU
            , Register (..)
            ) where

import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Extras (view)

import GHC.Word (Word16 (..))
import Data.Bits

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

newDCPU16 :: DCPU Word16
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



data BasicOpcode = SET
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | MOD
                 | SHL
                 | SHR
                 | AND
                 | BOR
                 | XOR
                 | IFE
                 | IFN
                 | IFG
                 | IFB
  deriving (Show, Eq, Enum)

data NonBasicOpcode = Reserved
                    | JSR
  deriving (Show, Eq, Enum)

data Instruction a = BasicInstruction BasicOpcode a a
                   | NonBasicInstruction NonBasicOpcode a
                   | UnknownInstruction Word16
  deriving (Eq)

instance Show a => Show (Instruction a) where
  show (BasicInstruction op a b) = unwords [show op, show a, show b]
  show (NonBasicInstruction op a) = unwords [show op, show a]
  show (UnknownInstruction w) = unwords ["Unknown instruction:", show w]

decodeInstruction :: Word16 -> Instruction Operand
decodeInstruction word = case opcode of
  -- Basic instructions
  0x01 -> BasicInstruction SET a b
  0x02 -> BasicInstruction ADD a b
  0x03 -> BasicInstruction SUB a b
  0x04 -> BasicInstruction MUL a b
  0x05 -> BasicInstruction DIV a b
  0x06 -> BasicInstruction MOD a b
  0x07 -> BasicInstruction SHL a b
  0x08 -> BasicInstruction SHR a b
  0x09 -> BasicInstruction AND a b
  0x0a -> BasicInstruction BOR a b
  0x0b -> BasicInstruction XOR a b
  0x0c -> BasicInstruction IFE a b
  0x0d -> BasicInstruction IFN a b
  0x0e -> BasicInstruction IFG a b
  0x0f -> BasicInstruction IFB a b
  -- Non-basic instructions
  0x00 -> case nonBasicOpcode of
    0x01 -> NonBasicInstruction JSR b
    _    -> UnknownInstruction word
  -- Unknown instruction
  _    -> UnknownInstruction word
  where
    opcode = word .&. 0xf
    nonBasicOpcode = (word `shiftR` 4) .&. 0x3f
    a = decodeOperand $ (word `shiftR` 4) .&. 0x3f
    b = decodeOperand $ (word `shiftR` 10) .&. 0x3f

encodeInstruction :: Instruction Operand -> Word16
encodeInstruction (BasicInstruction opcode a b) = bbbbbb .|. aaaaaa .|. oooo
  where
    aaaaaa = encodeOperand a `shiftL` 4
    bbbbbb = encodeOperand b `shiftL` 10
    oooo   = case opcode of
      SET -> 0x01
      ADD -> 0x02
      SUB -> 0x03
      MUL -> 0x04
      DIV -> 0x05
      MOD -> 0x06
      SHL -> 0x07
      SHR -> 0x08
      AND -> 0x09
      BOR -> 0x0a
      XOR -> 0x0b
      IFE -> 0x0c
      IFN -> 0x0d
      IFG -> 0x0e
      IFB -> 0x0f
encodeInstruction (NonBasicInstruction opcode a) = aaaaaa .|. oooooo
  where
    aaaaaa = encodeOperand a `shiftL` 10
    oooooo = (`shiftL` 4) $ case opcode of
      JSR -> 0x01
      Reserved -> 0x00
encodeInstruction (UnknownInstruction word) = word


data Operand = OpRegister Register
             | OpRegisterPointer Register
             | OpNextWordPlusRegisterPointer Register
             | OpPop
             | OpPeek
             | OpPush
             | OpSp
             | OpPc
             | OpO
             | OpNextWordPointer
             | OpNextWordLiteral
             | OpLiteral Word16
  deriving (Eq)

instance Show Operand where
  show (OpRegister r) = show r
  show (OpRegisterPointer r) = "[" <> show r <> "]"
  show (OpNextWordPlusRegisterPointer r) = "[next word + " <> show r <> "]"
  show OpPop = "POP"
  show OpPeek = "PEEK"
  show OpPush = "PUSH"
  show OpSp = "SP"
  show OpPc = "PC"
  show OpO = "O"
  show OpNextWordPointer = "[next word]"
  show OpNextWordLiteral = "(next word)"
  show (OpLiteral w) = show w

decodeOperand :: Word16 -> Operand
decodeOperand word
  | word <= 0x07 = OpRegister $ reg word
  | word <= 0x0f = OpRegisterPointer $ reg $ word - 0x08
  | word <= 0x17 = OpNextWordPlusRegisterPointer $ reg $ word - 0x10
  | word >= 0x20 = OpLiteral $ word - 0x20
  | otherwise = case word of
      0x18 -> OpPop
      0x19 -> OpPeek
      0x1a -> OpPush
      0x1b -> OpSp
      0x1c -> OpPc
      0x1d -> OpO
      0x1e -> OpNextWordPointer
      0x1f -> OpNextWordLiteral
      _    -> error $ "Unkown operand: " <> show word
  where
    reg = toEnum . fromIntegral

encodeOperand :: Operand -> Word16
encodeOperand operand = case operand of
  OpRegister r -> unreg r
  OpRegisterPointer r -> 0x08 + unreg r
  OpNextWordPlusRegisterPointer r -> 0x10 + unreg r
  OpPop -> 0x18
  OpPeek -> 0x19
  OpPush -> 0x1a
  OpSp -> 0x1b
  OpPc -> 0x1c
  OpO -> 0x1d
  OpNextWordPointer -> 0x1e
  OpNextWordLiteral -> 0x1f
  OpLiteral w -> 0x20 + w -- potential bug if x > 0x1f
  where
    unreg = fromIntegral . fromEnum

operandCycles :: Operand -> Int
operandCycles (OpNextWordPlusRegisterPointer _) = 1
operandCycles OpNextWordPointer = 1
operandCycles OpNextWordLiteral = 1
operandCycles _ = 0


--  decodeInstruction :: Word16 -> Instruction


-- data Address
--     = Pc
--     | Sp
--     | O
--     | Cycles
--     | Register Register
--     | Ram Word16
--     deriving (Eq)

-- instance Show Address where
--     show Pc           = "Pc"
--     show Sp           = "Sp"
--     show O            = "O"
--     show Cycles       = "Cycles"
--     show (Register r) = show r
--     show (Ram r)      = "[" ++ show r ++ "]"