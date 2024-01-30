module PreludeConclude (run) where

import X86
import Regs

prelude :: Int -> [Instr]
prelude size
    | size == 0 = [Pushq $ AReg Rbp]
    | otherwise =
        [ Pushq $ AReg Rbp,
          Movq (AReg Rsp) $ AReg Rbp,
          Subq (AImm size) $ AReg Rsp ]

conclusion :: Int -> [Instr]
conclusion size
    | size == 0 =
        [ Popq $ AReg Rbp,
          Retq ]
    | otherwise =
        [ Addq (AImm size) $ AReg Rsp,
          Popq $ AReg Rbp,
          Retq ]

run :: X86.Program -> X86.Program
run (Program size instrs) =
    let allInstrs = prelude size ++ instrs ++ conclusion size
    in Program size allInstrs