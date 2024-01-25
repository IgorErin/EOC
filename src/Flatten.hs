{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flatten (run) where

import qualified R1 as R
import qualified C0 as C

import Ident (Ident, Seed, newTemp)

import Control.Monad.State

import Control.Lens

data Ctx = Ctx {
    _ctxAssigns :: [C.Stmt],
    _ctxSeed :: Seed
}

$(makeLenses ''Ctx)

initCtx :: Seed -> Ctx
initCtx sd = Ctx {
    _ctxAssigns = [],
    _ctxSeed = sd
}

run :: R.Program -> C.Program
run (R.Program { R.seed, R.body}) =
    let st = fetchArg body
        (arg, ctx) = runState st $ initCtx seed
        seed' = view ctxSeed ctx
        stms = view ctxAssigns ctx & reverse
    in C.Program {
        C.seed = seed',
        C.body =  stms,
        C.result = arg
    }

fetchExpr :: R.Expr -> State Ctx C.Expr
fetchExpr (R.EInt n)              = return $ C.EArg $ C.AInt n
fetchExpr R.ERead                 = return C.ERead
fetchExpr (R.ESub expr)           = do
    arg <- fetchArg expr

    return $ C.ESub arg
fetchExpr (R.EAdd left right)     = do
    leftArg <- fetchArg left
    rightArg <- fetchArg right

    return $ C.EAdd leftArg rightArg
fetchExpr (R.ELet name expr body) = do
    expr' <- fetchExpr expr
    addAssign name expr'

    fetchExpr body
fetchExpr (R.EIdent name)        = return $ C.EArg $ C.AVar name

fetchArg :: R.Expr -> State Ctx C.Arg
fetchArg (R.EInt n)              = return $ C.AInt n
fetchArg R.ERead                 = do
    name <- nameExpr C.ERead

    return $ C.AVar name
fetchArg (R.ESub expr)           = do
    arg <- fetchArg expr
    name <- nameExpr $ C.ESub arg

    return $ C.AVar name
fetchArg (R.EAdd left right)     = do
    leftName <- fetchArg left
    rightName <- fetchArg right

    name <- nameExpr $ C.EAdd leftName rightName

    return $ C.AVar name
fetchArg (R.ELet name expr body) = do
    expr' <- fetchExpr expr
    addAssign name expr'

    fetchArg body
fetchArg (R.EIdent name)         = return $ C.AVar name

nameExpr :: C.Expr -> State Ctx Ident
nameExpr expr = do
    name <- zoom ctxSeed newTemp
    addAssign name expr

    return name

addAssign :: Ident -> C.Expr -> State Ctx ()
addAssign name expr = modify $ over ctxAssigns (C.SAssign name expr :)
