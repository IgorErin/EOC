module X86 (Label, Reg (..), Arg (..), Instr (..), Program (..)) where

type Label = String

data Reg =
    RSP
    | RBP
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Show, Eq)

data Arg =
    AInt Int
    | AReg Reg
    | ADeref Int Reg
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

data Program = Program Int [Instr] deriving (Show, Eq)
