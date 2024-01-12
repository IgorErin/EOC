{-# LANGUAGE TemplateHaskell #-}

module Uniquify (run) where

import R1 as R ( Expr(..), Ident, Program(..) )

import Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Lens

newName :: [R.Ident] -> Ident -> Ident
newName e current =
    let newNameWith :: Int -> Ident -> Ident
        newNameWith count name =
            let numbered = (name ++ show count) in
            if numbered `elem` e
            then newNameWith (count + 1) name
            else numbered
    in newNameWith 0 current

data Names = Names { _old_names:: [R.Ident], _names_map:: Map Ident Ident }

$(makeLenses ''Names)

initNames = Names { _old_names = [], _names_map = Map.empty }

runExpr' :: Expr -> StateT Names (Except String) Expr
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

    name' <- case Map.lookup name nmap of
            Just x ->  return x
            Nothing -> throwError $ "name not found" ++ name

    return $ EIdent name'

run :: Program -> Program
run (Program expr) =
    let mb =  evalStateT (runExpr' expr) initNames
        eitherResult =runIdentity $ runExceptT mb
    in case eitherResult of
        Left m -> error m
        Right v -> Program v