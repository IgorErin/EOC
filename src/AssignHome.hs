{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module AssignHome (run) where

import qualified X86V as XV
import qualified X86 as X

import Frame as F
import Ident
import Regs

import Control.Monad.State
import Control.Lens

import Data.Map as Map ( insert, lookup, Map , empty )

data Ctx = Ctx {
    _frame :: Frame,
    _mname :: Map Ident Int
}

initCtx :: Ctx
initCtx = Ctx {
    _frame = F.empty,
    _mname = Map.empty
}

$(makeLenses ''Ctx)

run :: XV.Program -> X.Program
run (XV.Program _ insts) =
    let s = mapM runInstr insts
        (insts', ctx) = runState s initCtx
        sSize = F.getSize $ view frame ctx
    in X.Program sSize insts'

runInstr :: XV.Instr -> State Ctx X.Instr
runInstr (XV.Addq left right) = do
    l <- runArg left
    r <- runArg right

    return $ X.Addq l r
runInstr (XV.Subq f s) = do
    f' <- runArg f
    s' <- runArg s

    return $ X.Subq f' s'
runInstr (XV.Negq arg) = do
    arg' <- runArg arg
    return $ X.Negq arg'
runInstr (XV.Movq left right) = do
    xleft <- runArg left
    xright <- runArg right

    return $ X.Movq xleft xright
runInstr (XV.Callq name) = return $ X.Callq name
runInstr (XV.Pushq arg) = do
    arg' <- runArg arg
    return $ X.Pushq arg'
runInstr (XV.Popq arg) = do
    arg' <- runArg arg
    return $ X.Popq arg'
runInstr XV.Retq = return X.Retq
runInstr (XV.Jmp l) = return $ X.Jmp l

runArg :: XV.Arg -> State Ctx X.Arg
runArg (XV.AImm n) = return $ X.AImm n
runArg (XV.AVar n) = do
    nmap' <- gets $ view mname

    case Map.lookup n nmap' of
        Just x' -> return $ X.ADeref x' Rbp
        Nothing -> do
            count <- zoom frame F.nextOffset
            modify (over mname $ Map.insert n count)

            return $ X.ADeref count Rbp
runArg (XV.AReg r) = return $ X.AReg r
