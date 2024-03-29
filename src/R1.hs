{-# LANGUAGE NamedFieldPuns #-}

module R1
    (Program(..), Expr(..),
    program,
    int, read, sub, read_, add,
    ident, let_)
    where

import Ident (Ident, Seed)

data Program = Program {
        body :: Expr,
        seed :: Ident.Seed
    } deriving (Show, Eq)

program :: Expr -> Seed -> Program
program body seed = Program { body , seed }

data Expr =
    EInt Int
    | ERead
    | ESub Expr
    | EAdd Expr Expr
    | ELet Ident Expr Expr
    | EIdent Ident
    deriving (Eq, Show)

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