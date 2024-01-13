module Inter (run) where

import Data.Map as Map
import Data.Function
import Data.Maybe (fromMaybe)

import R1 ( Program(..), Expr(..), Ident )

run :: [Int] -> Program -> Int
run input (Program expr) = runExpr input Map.empty expr

runExpr :: [Int] -> Map Ident Int -> Expr -> Int
runExpr _ _ (EInt n) = n
runExpr (hd : _) _ ERead = hd
runExpr [] _ ERead = error "Not enough input for read"
runExpr i d (EAdd left right) =
    let left' = runExpr i d left
        right' = runExpr i d right
    in left' + right'
runExpr i d (ESub expr) = - (runExpr i d expr)
runExpr i d (ELet name value body) =
    let value' = runExpr i d value
        d' = Map.insert name value' d
    in runExpr i d' body
runExpr _ d (EIdent name) = Map.lookup name d & fromMaybe (error $ name ++ " not found")