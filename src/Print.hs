module Print (run) where

import RV32 as RV
-- import Data.Char (toLower)
-- import Data.Function ((&))

run :: RV.Program -> String
run = undefined
-- run :: Program -> String
-- run (Program count instr) =
--     let allProgram =
--             prelude count
--             ++ instr
--             ++ exterlude count

--         allStr = allProgram & concatMap (\ x -> sp ++ runInstr x ++ "\n")

--         start =
--             "    .globl main\n\
--             \main:\n"
--     in start ++ allStr

-- prelude :: Int -> [Instr]
-- prelude n = [
--     Pushq (AReg RBP),
--     Movq (AReg RSP) (AReg RBP),
--     Subq (AInt n) (AReg RSP) ]

-- exterlude :: Int -> [Instr]
-- exterlude n = [
--     -- Movq (AReg RAX) (AReg RDI),
--     -- Callq "print_int",
--     Addq (AInt n) (AReg RSP),
--     -- Movq (AInt 0) (AReg RAX),
--     Popq (AReg RBP),
--     Retq ]

-- sp :: String
-- sp = replicate 4 ' ' :: String

-- runInstr :: Instr -> String
-- runInstr Retq = "retq"
-- runInstr (Popq arg) = "popq" ++ sp ++ runArg arg
-- runInstr (Pushq arg) = "pushq" ++ sp ++ runArg arg
-- runInstr (Callq lb) = "callq" ++ sp ++ lb
-- runInstr (Movq left right) =
--     "movq" ++ sp ++ runArg left ++ ", " ++ runArg right
-- runInstr (Negq arg) = "negq" ++ sp ++ runArg arg
-- runInstr (Subq left right) =
--     "subq" ++ sp ++ runArg left ++ ", " ++ runArg right
-- runInstr (Addq left right) =
--     "addq" ++ sp ++ runArg left ++ ", " ++ runArg right

-- runArg :: Arg -> String
-- runArg (AInt n) = "$" ++ show n
-- runArg (AReg reg) = "%" ++ runReg reg
-- runArg (ADeref offset reg) =
--     show offset ++ "(%" ++ runReg reg ++ ")"

-- runReg :: Reg -> String
-- runReg reg = show reg & map toLower
