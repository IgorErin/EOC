module Uniquify (run) where

import R1 as R

import Data.Map as Map
import Data.Function
import Data.Maybe

import Control.Monad.State

run :: Program -> Program
run (Program expr) =
    let ninit = Names { _names = [], _names_map = Map.empty }
        result = evalState (runExpr' expr) ninit
    in Program result

newName :: [R.Ident] -> Ident -> Ident
newName e current =
    let newNameWith :: Int -> Ident -> Ident
        newNameWith count name =
            let numbered = (name ++ show count) in
            if numbered `elem` e
            then newNameWith (count + 1) name
            else numbered
    in newNameWith 0 current
    
data Names = Names { _names:: [R.Ident], _names_map:: Map Ident Ident }

runExpr' :: Expr -> State Names Expr
runExpr' n@(EInt _) = do return n
runExpr' ERead = do return ERead
runExpr' (ESub expr) = do
    expr' <- runExpr' expr

    return $ ESub expr'
runExpr' (EAdd left right) = do
    left' <- runExpr' left
    right' <- runExpr' right

    return $ EAdd left' right'
runExpr' (ELet name expr body) = do
    nmap <- gets _names_map
    old <- gets _names

    let name' = newName old name
    let map' = Map.insert name name' nmap
    let old' = name' : old

    modify (\ x ->  x { _names = old' }) -- lenses
    expr' <- runExpr' expr

    modify (\ x -> x {_names_map = map'})
    body' <- runExpr' body

    return (ELet name' expr' body')
runExpr' (EIdent name) = do
    nmap <- gets _names_map
    let name' = Map.lookup name nmap & fromMaybe (error $ "Not found" ++ name) -- Transformer with Error

    return $ EIdent name'