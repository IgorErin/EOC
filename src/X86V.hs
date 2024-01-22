module X86V (
    Arg (..), Instr (..),
    Program (..)
    ) where

import Ident (Ident)
import Regs ( Reg )

type Label = String

data Arg =
    AImm Int
    | AVar Ident
    | AReg Reg
    deriving (Show, Eq)

data Instr =
    Addq Arg Arg
    | Subq Arg Arg
    | Negq Arg
    | Movq Arg Arg
    | Pushq Arg
    | Popq Arg
    | Callq Label
    | Retq
    | Jmp Label
    deriving (Show, Eq)

data Program = Program [Ident] [Instr] deriving (Show, Eq)
