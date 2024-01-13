module X86V
    (Label, Ident,
     Reg (..), Arg (..), Instr (..), Program (..)
    ) where

type Label = String
type Ident = String

data Reg = RAX
    deriving (Show, Eq)

data Arg =
    AInt Int
    | AVar Ident
    | AReg Reg
    deriving (Show, Eq)

data Instr =
    Addq Arg Arg
    | Subq Arg Arg
    | Negq Arg
    | Movq Arg Arg
    | Callq Label
    | Pushq Arg
    | Popq Arg
    | Retq
    deriving (Show, Eq)

data Program = Program [Ident] [Instr] deriving (Show, Eq)
