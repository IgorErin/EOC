module InstrPatch (run) where

import X86 as X
import Regs

run :: X.Program -> X.Program
run (Program n insts) = Program n $ concatMap runInstr insts

runInstr :: X.Instr -> [X.Instr]
runInstr (Movq left right) = runBoth Movq left right
runInstr x = [x]

runBoth :: (X.Arg -> X.Arg -> X.Instr) -> X.Arg -> X.Arg -> [Instr]
runBoth cons left@(ADeref _ _) right@(ADeref _ _) =
    [ Movq left (AReg Rax),
      cons (AReg Rax) right ]
runBoth cons left right = [cons left right]