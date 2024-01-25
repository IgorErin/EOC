{-# LANGUAGE InstanceSigs #-}

module C0 (Arg(..), Expr (..), Stmt (..), Program (..)) where

import Ident (Ident, Seed)
import Fmt ((+||), (||+))

data Arg =
    AInt Int
    | AVar Ident
    deriving (Eq)

instance Show Arg where
    show :: Arg -> String
    show (AInt n) = show n
    show (AVar i) = show i

data Expr =
    EArg Arg
    | ERead
    | ESub Arg
    | EAdd Arg Arg
    deriving (Eq)

instance Show Expr where
    show :: Expr -> String
    show (EArg arg) = show arg
    show ERead = "read"
    show (ESub arg) = "(-"+||arg||+")"
    show (EAdd left right) = "(+"+||left||+","+||right||+")"

data Stmt =
    SAssign Ident Expr
    deriving (Eq)

instance Show Stmt where
    show :: Stmt -> String
    show (SAssign ident expr) = ""+||ident||+" := "+||expr||+""

data Program = Program {
    seed :: Seed,
    body :: [Stmt],
    result :: Arg }
     deriving (Eq, Show)
