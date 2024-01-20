module RV32 (Offset, Reg (..), Instr (..), Program (..)) where

import Ident (Ident)
import Regs ( Reg(..) )

type Offset = Int
type Imm = Int

data Instr =
    Add Reg Reg Reg
    | Addi Reg Reg Imm
    | Sub Reg Reg Reg
    | Subi Reg Reg Imm
    | Li Reg Imm
    | Sw Reg Offset Reg
    | Lw Reg Offset Reg
    | Jal Ident
    | Mov Reg Reg
    | Neg Reg Reg
    | Ret
    deriving (Show, Eq)

data Program = Program {
    frameSize :: Int,
    code :: [Instr]
} deriving (Show, Eq)
