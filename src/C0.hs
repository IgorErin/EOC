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
    deriving (Eq, Show)

data Program = Program {
     names :: [Ident],
     body :: [Stmt],
     result :: Arg
  }  deriving (Eq, Show)
