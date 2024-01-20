module C0 (Arg(..), Expr (..), Stmt (..), Program (..)) where

import Ident (Ident)

data Arg =
    AInt Int
    | AVar Ident
    deriving (Eq, Show)

data Expr =
    EArg Arg
    | ERead
    | ESub Arg
    | EAdd Arg Arg
    deriving (Eq, Show)

data Stmt =
    SAssign Ident Expr
    | SReturn Arg
    deriving (Eq, Show)

data Program = Program [Ident] [Stmt] deriving (Eq, Show)
