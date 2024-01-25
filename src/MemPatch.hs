module MemPatch (run) where

import X86
import Regs

run :: X86.Program -> X86.Program
run (Program stackSize instrs) =
    Program stackSize $ concatMap runInstr instrs

runInstr :: Instr -> [Instr]
runInstr (Addq left right) = runBin left right Addq
runInstr (Subq left right) = runBin left right Subq
runInstr (Movq left right) = runBin left right Movq
runInstr x                 = [x]

immBound :: Int
immBound = 2 ^ (16 :: Int)

toRaxAndBack :: Arg -> Arg -> (Arg -> Arg -> Instr) -> [Instr]
toRaxAndBack left right cons =
    [ Movq left $ AReg Rax,
      cons (AReg Rax) right ]

runBin :: Arg -> Arg -> (Arg -> Arg -> Instr) -> [Instr]
runBin left@(ADeref _ _) right@(ADeref _ _ ) cons = toRaxAndBack left right cons
runBin left@(AImm imm) right@(ADeref _ _ ) cons
    | imm >= immBound = toRaxAndBack left right cons
    | otherwise = [ cons left right ]
runBin left@(ADeref _ _) right@(AImm imm) cons
    | imm >= immBound = toRaxAndBack left right cons
    | otherwise = [ cons left right ]
runBin left right cons = [cons left right]