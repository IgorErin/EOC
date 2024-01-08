module R1
    (Program(..), Expr(..), Ident,
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
    | ELet Ident Expr Expr
    | EIdent Ident
    deriving (Show, Eq)

ident :: Ident -> Expr
ident = EIdent

let_:: Ident -> Expr -> Expr -> Expr
let_ = ELet

int :: Int -> Expr
int = EInt

read_ :: Expr
read_ = ERead

sub :: Expr -> Expr
sub = ESub

add :: Expr -> Expr -> Expr
add = EAdd