module ISelect (run) where

import qualified C0 as C
import qualified X86V as X

import Control.Monad.Writer
    ( MonadWriter(tell), execWriter, Writer )

run :: C.Program -> X.Program
run (C.Program vars stmts) =
    let insts = execWriter $ mapM runStmt stmts
    in X.Program vars insts

runStmt :: C.Stmt -> Writer [X.Instr] ()
runStmt (C.SAssign name expr) = tell $ runAssign name expr
runStmt (C.SReturn arg) =
    tell [ X.Movq (X.AReg X.RAX) $ runArg arg ]

runAssign :: C.Ident -> C.Expr -> [X.Instr]
runAssign name (C.EArg arg) =
    [ X.Movq (X.AVar name) $ runArg arg ]
runAssign name (C.ESub arg) =
    [ X.Movq (X.AVar name) $ runArg arg ,
      X.Subq (X.AVar name) ]
runAssign name (C.EAdd left right) = runAdd name left right
runAssign name C.ERead =
    [ X.Callq "read_int",
      X.Movq (X.AVar name) (X.AReg X.RAX) ]

runAdd :: C.Ident -> C.Arg -> C.Arg -> [X.Instr]
runAdd name ((C.AVar leftVar)) rightArg | name == leftVar =
    [ X.Addq (X.AVar name) $ runArg rightArg ]
runAdd name leftArg ((C.AVar rightVar)) | name == rightVar =
    [ X.Addq (X.AVar name) $ runArg leftArg ]
runAdd name left right =
    [ X.Movq (X.AVar name) $ runArg left,
      X.Addq (X.AVar name) $ runArg right ]

runArg :: C.Arg -> X.Arg
runArg (C.AVar n) = X.AVar n
runArg (C.AInt n) = X.AInt n

zero :: X.Arg
zero = X.AInt 0

