module Ast
    (Program(..), Expr(..),
    program,
    int, read, sub, read_, add) 
    where 

newtype Program = Program Expr deriving (Show, Eq)

program :: Expr -> Program 
program = Program 

data Expr = 
    EInt Int 
    | ERead 
    | ESub Expr
    | EAdd Expr Expr
    deriving (Show, Eq)

int :: Int -> Expr
int = EInt 

read_ :: Expr
read_ = ERead 

sub :: Expr -> Expr
sub = ESub 

add :: Expr -> Expr -> Expr
add = EAdd 