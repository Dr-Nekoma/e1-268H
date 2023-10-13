module Instruction ( Instruction (..)
                   , Operand (..)
                   , decodeInstruction
                   , encodeInstruction
                   ) where

import Data.Word
import Data.Bits

import DCPU (Register)

data Instruction a = BasicInstruction BasicOpcode a a
                   | NonBasicInstruction NonBasicOpcode a
                   | UnknownInstruction Word16
  deriving (Eq)

instance Show a => Show (Instruction a) where
  show (BasicInstruction op a b) = unwords [show op, show a, show b]
  show (NonBasicInstruction op a) = unwords [show op, show a]
  show (UnknownInstruction w) = unwords ["Unknown instruction:", show w]


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
  deriving (Show, Eq, Bounded, Enum)

data NonBasicOpcode = Reserved
                    | JSR
  deriving (Show, Eq, Enum)


decodeInstruction :: Word16 -> Instruction Operand
decodeInstruction word = case opcode of
  -- Basic instructions
  0x1 -> BasicInstruction SET a b
  0x2 -> BasicInstruction ADD a b
  0x3 -> BasicInstruction SUB a b
  0x4 -> BasicInstruction MUL a b
  0x5 -> BasicInstruction DIV a b
  0x6 -> BasicInstruction MOD a b
  0x7 -> BasicInstruction SHL a b
  0x8 -> BasicInstruction SHR a b
  0x9 -> BasicInstruction AND a b
  0xa -> BasicInstruction BOR a b
  0xb -> BasicInstruction XOR a b
  0xc -> BasicInstruction IFE a b
  0xd -> BasicInstruction IFN a b
  0xe -> BasicInstruction IFG a b
  0xf -> BasicInstruction IFB a b
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

-- operandCycles :: Operand -> Int
-- operandCycles (OpNextWordPlusRegisterPointer _) = 1
-- operandCycles OpNextWordPointer = 1
-- operandCycles OpNextWordLiteral = 1
-- operandCycles _ = 0
