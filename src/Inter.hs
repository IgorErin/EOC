module Inter (run) where

import Data.Map as Map
import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Ident (Ident)

import R1 ( Program(..), Expr(..))

run :: Program -> IO Int
run (Program expr _) = runExpr Map.empty expr

runExpr :: Map Ident Int -> Expr -> IO Int
runExpr _ (EInt n) = return n
runExpr _ ERead = do read <$> getLine
runExpr d (EAdd left right) = do
    left' <- runExpr d left
    right' <- runExpr d right

    return $ left' + right'
runExpr d (ESub expr) =  negate <$> runExpr d expr
runExpr d (ELet name value body) = do -- TODO use NoFieldSelectors
    value' <- runExpr d value
    let d' = Map.insert name value' d

    runExpr d' body
runExpr d (EIdent name) =
    Map.lookup name d
    & fromMaybe (error "not found")
    & return

