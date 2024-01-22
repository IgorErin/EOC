module Regs (Reg (..)) where

data Reg =
    Rsp
    | Rbp
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsi
    | Rdi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Show, Eq)