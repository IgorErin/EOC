{-# LANGUAGE TemplateHaskell #-}

module Uniquify (run) where

import R1 as R ( Expr(..), Program(..) )

import Ident (Ident, newIdent)

import Data.Map as Map (empty, insert, lookup, Map)
import Data.Maybe (fromMaybe)

import Control.Monad.State (gets, modify, evalState, State)
import Control.Lens ((&), set, makeLenses)

data Names = Names { _old_names:: [Ident], _names_map:: Map Ident Ident }

$(makeLenses ''Names)

initNames :: Names
initNames = Names { _old_names = [], _names_map = Map.empty }

newName :: [Ident] -> Ident -> Ident
newName e current =
    let newNameWith :: Int -> Ident -> Ident
        newNameWith count name =
            let numbered = newIdent count name in
            if numbered `elem` e
            then newNameWith (count + 1) name
            else numbered
    in newNameWith 0 current

runExpr' :: Expr -> State Names  Expr
runExpr' n@(EInt _) = do return n
runExpr' ERead = do return ERead
runExpr' (ESub expr) = do
    expr' <- runExpr' expr

    return $ ESub expr'
runExpr' (EAdd left right) = do
    left_ <- runExpr' left
    right_ <- runExpr' right

    return $ EAdd left_ right_
runExpr' (ELet name expr body) = do
    nmap <- gets _names_map
    old <- gets _old_names

    let name' = newName old name
    let map' = Map.insert name name' nmap
    let old' = name': name : old

    modify $ set old_names old'
    expr' <- runExpr' expr

    modify $ set names_map map'
    body' <- runExpr' body

    return (ELet name' expr' body')
runExpr' (EIdent name) = do
    nmap <- gets _names_map

    let name' = Map.lookup name nmap
                & fromMaybe (error "name not found")

    return $ EIdent name'

run :: Program -> Program
run (Program expr) = Program $ evalState (runExpr' expr) initNames