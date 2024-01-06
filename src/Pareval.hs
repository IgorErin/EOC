module Pareval (run) where 

import Ast as A

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

run :: A.Program -> A.Program 
run (Program expr) = 
    let (expr', num) = fetch expr 
        result = case expr' of 
            Just e -> 
                if num /= 0 
                then EAdd e $ EInt num 
                else e 
            Nothing -> EInt num
    in Program result
            
