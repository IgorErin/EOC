module Print (run) where

import RV32
import Regs

import Fmt ((+|), (|+), (+||), (||+), Builder, pretty)
import Data.Text.Lazy (Text)

run :: Program -> Text
run (Program count instr) =
    let instr' = prelude count ++ instr ++ exterlude count
        body = mconcat $ map textInstrWithOffset instr'
        header = "    .global main\nmain:\n"
    in pretty $ header <> body

-- TODO move to appropriate place
prelude :: Int -> [Instr]
prelude stackSize = [ Addi Sp Sp (-stackSize)]

exterlude :: Int -> [Instr]
exterlude stackSize = [ Addi Sp Sp stackSize, Ret ]

textInstrWithOffset :: Instr -> Builder
textInstrWithOffset x = "    "+|textInstr x|+""

textInstr :: Instr -> Builder
textInstr (Add rd rs1 rs2)  = "add  "+||rd||+", "+||rs1||+", "+||rs2||+"\n"
textInstr (Addi rd rs1 rs2) = "addi "+||rd||+", "+||rs1||+", "+||rs2||+"\n"
textInstr (Sub rd rs1 rs2)  = "sub  "+||rd||+", "+||rs1||+", "+||rs2||+"\n"
textInstr (Subi rd rs1 rs2) = "subi "+||rd||+", "+||rs1||+", "+||rs2||+"\n"
textInstr (Li rd imm)       = "li   "+||rd||+", "+|imm|+"\n"
textInstr (Sw rd off rs)    = "sw   "+||rd||+", "+|off|+"("+||rs||+")\n"
textInstr (Lw rd off rs)    = "lw   "+||rd||+", "+|off|+"("+||rs||+")\n"
textInstr (Jal lb)          = "jal  "+||lb||+"\n"
textInstr (Mov rd rs)       = "mov  "+||rd||+", "+||rs||+"\n"
textInstr (Neg rd rs)       = "neg  "+||rd||+", "+||rs||+"\n"
textInstr Ret               = "ret\n"