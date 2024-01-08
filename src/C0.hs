module C0 (Ident, Arg(..), Expr (..), Stmt (..), Program (..)) where

type Ident = String

data Arg = AInt Int | AVar Ident

data Expr = EArg Arg | ERead | ESub Arg | EAdd Arg Arg

data Stmt = SAssign Ident Expr | Return Arg

data Program = Program [Ident] [Stmt]