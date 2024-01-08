module Uniquify (run) where

import Ast as A

import Data.Map as Map
import Data.Function
import Data.Maybe

run :: Program -> Program
run (Program expr) = Program $ fst $ runExpr [] Map.empty expr

newName :: [A.Ident] -> Ident -> Ident
newName e current =
    let newNameWith :: Int -> Ident -> Ident
        newNameWith count name =
            let numbered = (name ++ show count) in
            if numbered `elem` e
            then newNameWith (count + 1) name
            else numbered
    in newNameWith 0 current

runExpr :: [A.Ident] -> Map Ident Ident -> Expr -> (Expr, [A.Ident])
runExpr e _ n@(EInt _) = (n, e)
runExpr e _ ERead = (ERead, e)
runExpr e m (ESub expr) =
    let (expr', e') = runExpr e m expr
    in (ESub expr', e')
runExpr e m (EAdd left right) =
    let (left', e') = runExpr e m left
        (right', e'') = runExpr e' m right
    in (EAdd left' right', e'')
runExpr e m (ELet name expr body) =
    let name' = newName e name
        m' = Map.insert name name' m
        e' = name' : e

        (expr', e'') = runExpr e' m expr
        (body', e''') = runExpr e'' m' body
    in (ELet name' expr' body', e''')
runExpr e m (EIdent name) =
    let name' = Map.lookup name m & fromMaybe (error $ "Name not found: " ++ name)
    in (EIdent name' , e)
