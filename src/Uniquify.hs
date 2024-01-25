{-# LANGUAGE TemplateHaskell #-}

module Uniquify (run) where

import qualified ParseTree as PT
import qualified R1 as R

import Control.Monad.State

import Control.Lens ( view, over, makeLenses)
import Control.Lens.Zoom (zoom)

import Data.Map (Map)
import qualified Data.Map as Map (insert, lookup, empty)
import Data.Maybe as Maybe (fromMaybe)

import Fmt

import qualified Ident (Seed, Ident, newTempWithText, initSeed)

data Ctx = Ctx {
    _nmap :: Map PT.Ident Ident.Ident,
    _seed :: Ident.Seed
}

initCtx :: Ctx
initCtx = Ctx { _nmap = Map.empty, _seed = Ident.initSeed}

$(makeLenses ''Ctx)

run :: PT.Program -> R.Program
run (PT.Program body) =
    let (expr, ctx) = runState (runExpr body) initCtx
    in R.program expr (view seed ctx)

runExpr :: PT.Expr -> State Ctx R.Expr
runExpr (PT.EInt n)    = return $ R.EInt n
runExpr PT.ERead       = return   R.ERead
runExpr (PT.ESub e)    = do
    e' <- runExpr e

    return $ R.ESub e'
runExpr (PT.EAdd l r) = do
    l' <- runExpr l
    r' <- runExpr r

    return $ R.EAdd l' r'
runExpr (PT.ELet name e body) = do
    name' <- zoom seed $ Ident.newTempWithText name
    modify $ over nmap $ Map.insert name name'

    e' <- runExpr e
    body' <- runExpr body

    return $ R.ELet name' e' body'
runExpr (PT.EIdent name) = do
    name' <- gets (Maybe.fromMaybe (error $ "name: "+|name|+" not found") . Map.lookup name . view nmap)

    return $ R.EIdent name'
