{-# LANGUAGE InstanceSigs #-}
module Regs (Reg (..)) where

data Reg =
    Zero
    | Ra
    | Sp
    | Gp
    | Tp
    | T0
    | T1
    | T2
    | Fp
    | S1
    | A0
    | A1
    | A2
    | A3
    | A4
    | A5
    | A6
    | A7
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | S8
    | S9
    | S10
    | S11
    | T3
    | T4
    | T5
    | T6
    deriving (Eq)

instance Show Reg where
    show :: Reg -> String
    show Zero   = "zero"
    show Ra     = "ra"
    show Sp     = "sp"
    show Gp     = "gp"
    show Tp     = "tp"
    show T0     = "t0"
    show T1     = "t1"
    show T2     = "t2"
    show Fp     = "fp"
    show S1     = "s1"
    show A0     = "a0"
    show A1     = "a1"
    show A2     = "a2"
    show A3     = "a3"
    show A4     = "a4"
    show A5     = "a5"
    show A6     = "a6"
    show A7     = "a7"
    show S2     = "s2"
    show S3     = "s3"
    show S4     = "s4"
    show S5     = "s5"
    show S6     = "s6"
    show S7     = "s7"
    show S8     = "s8"
    show S9     = "s9"
    show S10    = "s10"
    show S11    = "s11"
    show T3     = "t3"
    show T4     = "t4"
    show T5     = "t5"
    show T6     = "t6"