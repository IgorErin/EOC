module Ast
    (Program(..), Expr(..),
    program,
    int, read, sub, read_, add,
    ident, let_)
    where

newtype Program = Program Expr deriving (Show, Eq)

program :: Expr -> Program
program = Program

type Ident = String

data Expr =
    EInt Int
    | ERead
    | ESub Expr
    | EAdd Expr Expr
    | ALet Ident Expr Expr
    | AIdent Ident
    deriving (Show, Eq)

ident :: Ident -> Expr
ident = AIdent

let_:: Ident -> Expr -> Expr -> Expr
let_ = ALet

int :: Int -> Expr
int = EInt

read_ :: Expr
read_ = ERead

sub :: Expr -> Expr
sub = ESub

add :: Expr -> Expr -> Expr
add = EAdd