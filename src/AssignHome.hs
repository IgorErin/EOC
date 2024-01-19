{-# LANGUAGE TemplateHaskell #-}

module AssignHome where

import qualified RV32
import qualified RV
import Regs

import Frame as F

import Control.Monad.State
import Control.Lens

import Data.Map as Map ( insert, lookup, Map , empty )

data Ctx = Ctx {
    _frame :: Frame,
    _mname :: Map RV.Ident Int
}

initCtx :: Ctx
initCtx = Ctx {
    _frame = F.empty,
    _mname = Map.empty
}

$(makeLenses ''Ctx)

run :: RV.Program -> RV32.Program
run (RV.Program _ insts) =
    let s = mapM runInstr insts
        (insts', ctx) = runState s initCtx
        fr = F.getSize $ view frame ctx
    in RV32.Program fr insts'

returnS :: RV32.Instr -> State Ctx [RV32.Instr]
returnS = return . (: [])

runInstr :: RV.Instr -> State Ctx RV32.Instr
runInstr (RV.Addi dest left imm) =
    return $ RV32.Addi dest left imm
runInstr (RV.Add dest left right) =
    return $ RV32.Add dest left right
runInstr (RV.Sub dest left right) =
    return $ RV32.Sub dest left right
runInstr (RV.Subi dest left imm) =
    return $ RV32.Subi dest left imm
runInstr (RV.Neg dest arg) =
    return $ RV32.Neg dest arg
runInstr (RV.Mov dest arg) =
    return $ RV32.Mov dest arg
runInstr (RV.Jal name) =
    return $ RV32.Jal name
runInstr RV.Ret = return RV32.Ret
runInstr (RV.Li dest imm) =
    return $ RV32.Li dest imm
runInstr (RV.Lw dest ident) = do
    (offset, reg) <- runIdent ident

    return $ RV32.Lw dest offset reg
runInstr (RV.Sw src ident) = do
    (offset, reg) <- runIdent ident

    return $ RV32.Sw src offset reg

runIdent :: RV.Ident ->  State Ctx (Int, Reg)
runIdent name = do
    nmap' <- gets $ view mname
    let x = Map.lookup name nmap'

    case x of
        Just x' -> return (x', Sp)
        Nothing -> do
            count <- zoom frame F.nextOffset
            modify (over mname $ Map.insert name count)

            return (count, Sp)
