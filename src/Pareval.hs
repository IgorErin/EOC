module Pareval (run) where

import Ast as A ( Program(..), Expr(..) )

both :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
both f (Just left) (Just right) = Just $ f left right
both _ Nothing right = right
both _ left Nothing = left

fetch :: Expr -> (Maybe Expr, Int)
fetch (EInt n)    = (Nothing, n)
fetch ERead       = (Just ERead, 0)
fetch (ESub expr) =
    let (expr', num) = fetch expr
    in (ESub <$> expr', - num)
fetch (EAdd leftExpr rightExpr) =
    let (leftExpr', leftNum) = fetch leftExpr
        (rightExpr', rightNum) = fetch rightExpr
        resultExpr = both EAdd leftExpr' rightExpr'
    in (resultExpr, leftNum + rightNum)
fetch x@(EIdent _) = (Just x, 0)
fetch (ELet name value body) =
    let (body', number) = fetch body
        value' = runExpr value
        resultExpr = ELet name value' <$> body'
    in (resultExpr, number)

runExpr :: A.Expr -> A.Expr
runExpr expr =
    let (expr', num) = fetch expr
    in case expr' of
        Just e ->
            if num /= 0
            then EAdd e $ EInt num
            else e
        Nothing -> EInt num

run :: A.Program -> A.Program
run (Program expr) = Program $ runExpr expr

