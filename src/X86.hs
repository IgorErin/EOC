{-# LANGUAGE InstanceSigs #-}
module X86 (Label, Arg (..), Instr (..), Program (..)) where

import Regs
import Fmt ((+||), (||+))

type Label = String

data Arg =
    AImm Int
    | AReg Reg
    | ADeref Int Reg
    deriving Eq

instance Show Arg where
    show :: Arg -> String
    show (AImm n) = "$"+||n||+""
    show (AReg reg) = "%"+||reg||+""
    show (ADeref off reg) = ""+||off||+"(%"+||reg||+")"


data Instr =
    Addq Arg Arg
    | Subq Arg Arg
    | Negq Arg
    | Movq Arg Arg
    | Callq Label
    | Pushq Arg
    | Popq Arg
    | Retq
    | Jmp Label
    deriving (Show, Eq)

data Program = Program Int [Instr] deriving (Show, Eq)