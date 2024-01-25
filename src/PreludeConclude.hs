module PreludeConclude (run) where

import X86
import Regs

prelude :: Int -> [Instr]
prelude size = [
    Pushq $ AReg Rbp,
    Subq (AImm size) $ AReg Rsp ]

conclusion :: Int -> [Instr]
conclusion size = [
    Addq (AImm size) $ AReg Rsp,
    Popq $ AReg Rbp ]

run :: X86.Program -> X86.Program
run (Program size instrs) =
    let allInstrs = prelude size ++ instrs ++ conclusion size
    in Program size allInstrs