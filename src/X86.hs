module X86 where

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

data Arg =
    AInt Int
    | AReg Reg
    | ADeref Int Reg

data Instr =
    Addq Arg Arg
    | Subq Arg Arg
    | Negq Arg
    | Movq Arg Arg
    | Callq Label
    | Pushq Arg
    | Popq Arg
    | Retq

data Prog = Program Int [Instr]
