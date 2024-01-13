{-# LANGUAGE TemplateHaskell #-}

module AssignHome where

import qualified X86V as XV
import qualified X86 as X

import Frame as F

import Control.Monad.State
import Control.Lens

import Data.Map as Map ( insert, lookup, Map , empty )

data Ctx = Ctx {
    _frame :: Frame,
    _mname :: Map XV.Ident Int
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
runInstr (XV.Subq arg) = do
    arg' <- runArg arg
    return $ X.Subq arg'
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

runArg :: XV.Arg -> State Ctx X.Arg
runArg (XV.AInt n) = return $ X.AInt n
runArg (XV.AVar n) = do
    nmap' <- gets $ view mname
    let x = Map.lookup n nmap'

    case x of
        Just x' -> return $ X.ADeref x' X.RBP
        Nothing -> do
            count <- zoom frame F.nextOffset
            modify (over mname $ Map.insert n count)

            return $ X.ADeref count X.RBP
runArg (XV.AReg XV.RAX) = return $ X.AReg X.RAX
