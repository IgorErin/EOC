{-# LANGUAGE TemplateHaskell #-}

module Flatten (run) where

import qualified R1 as R
import qualified C0 as C

import Ident (Ident, newIdent)

import Control.Monad.State
import Control.Monad.Writer

import Control.Lens

data Ctx = Ctx {
    _assigns :: [C.Stmt],
    _vars :: [Ident],
    _count :: Int
}

$(makeLenses ''Ctx)

initCtx :: R.Expr -> Ctx
initCtx expr = Ctx {
    _assigns = [],
    _vars = execWriter $ getVars expr,
    _count  = 0
}

run :: R.Program -> C.Program
run (R.Program expr) =
    let st = fetchArg expr
        initCtx' = initCtx expr

        (arg, ctx) = runState st initCtx'
        stms = view assigns ctx & reverse
    in C.Program {
        C.names = view vars ctx,
        C.body =  stms,
        C.result = arg }

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
    name <- newName
    addAssign name expr

    return name


addAssign :: Ident -> C.Expr -> State Ctx ()
addAssign name expr = modify $ over assigns (C.SAssign name expr :)

type Names = [Ident]

getVars :: R.Expr -> Writer Names ()
getVars (R.EInt _)              = return ()
getVars R.ERead                 = return ()
getVars (R.ESub expr)           = getVars expr
getVars (R.EAdd left right)     = do
    getVars left
    getVars right
getVars (R.ELet name expr body) = do
    tell [name]

    getVars expr
    getVars body
getVars (R.EIdent _)         = return ()

newName :: State Ctx Ident
newName = do
    vars' <- gets (view vars)
    count' <- gets (view count)

    let name = newIdent count' "t"
    modify $ over count succ

    if name `elem` vars'
    then newName
    else do
        modify $ over vars (name :)
        return name
