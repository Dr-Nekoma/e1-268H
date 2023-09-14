module CPU (module CPU) where

import GHC.Word (Word16 (..))


data CPU = CPU { regA  :: Word16
               , regB  :: Word16
               , regC  :: Word16
               , regX  :: Word16
               , regY  :: Word16
               , regZ  :: Word16
               , regI  :: Word16
               , regJ  :: Word16
               , regPC :: Word16
               , regSP :: Word16
               , regO  :: Word16
               }
    deriving (Eq, Show)

newCpu :: CPU
newCpu = CPU { regA  = 0
             , regB  = 0
             , regC  = 0
             , regX  = 0
             , regY  = 0
             , regZ  = 0
             , regI  = 0
             , regJ  = 0
             , regPC = 0
             , regSP = 0xFFFF
             , regO  = 0
             }

data Register = A | B | C | X | Y | Z | I | J | PC | SP | O
  deriving (Show, Eq, Enum)

loadCPU :: CPU -> Register -> Word16
loadCPU cpu A  = regA  cpu
loadCPU cpu B  = regB  cpu
loadCPU cpu C  = regC  cpu
loadCPU cpu X  = regX  cpu
loadCPU cpu Y  = regY  cpu
loadCPU cpu Z  = regZ  cpu
loadCPU cpu I  = regI  cpu
loadCPU cpu J  = regJ  cpu
loadCPU cpu PC = regPC cpu
loadCPU cpu SP = regSP cpu
loadCPU cpu O  = regO  cpu

storeCPU :: CPU -> Register -> Word16 -> CPU
storeCPU cpu A  dat = cpu { regA  = dat }
storeCPU cpu B  dat = cpu { regB  = dat }
storeCPU cpu C  dat = cpu { regC  = dat }
storeCPU cpu X  dat = cpu { regX  = dat }
storeCPU cpu Y  dat = cpu { regY  = dat }
storeCPU cpu Z  dat = cpu { regZ  = dat }
storeCPU cpu I  dat = cpu { regI  = dat }
storeCPU cpu J  dat = cpu { regJ  = dat }
storeCPU cpu PC dat = cpu { regPC = dat }
storeCPU cpu SP dat = cpu { regSP = dat }
storeCPU cpu O  dat = cpu { regO  = dat }


data Opcode = NonBasic
            | SET
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

data Instruction a = Format1 Opcode a a
                   | Format2 NonBasicOpcode a
                   | Unknown Word16
  deriving (Eq)

instance Show a => Show (Instruction a) where
  show (Format1 op a b) = unwords [show op, show a, show b]
  show (Format2 op a) = unwords [show op, show a]
  show (Unknown w) = unwords ["Unknown instruction:", show w]

decodeInstruction :: Word16 -> Instruction Operand
decodeInstruction = undefined

encodeInstruction :: Instruction Operand -> Word16
encodeInstruction = undefined


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
