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
    tell [ X.Movq (runArg arg) (X.AReg X.RAX) ]

runAssign :: C.Ident -> C.Expr -> [X.Instr]
runAssign name (C.EArg arg) =
    [ X.Movq (runArg arg) (X.AVar name)  ]
runAssign name (C.ESub (C.AInt n)) =
    [ X.Movq (X.AInt (-n)) (X.AVar name) ]
runAssign name (C.ESub (C.AVar n)) =
    let dest = X.AVar name in
    [ X.Movq (X.AInt 0) dest,
      X.Subq (X.AVar n) dest ]
runAssign name (C.EAdd left right) = runAdd name left right
runAssign name C.ERead =
    [ X.Callq "read_int",
      X.Movq (X.AReg X.RAX) (X.AVar name) ]

runAdd :: C.Ident -> C.Arg -> C.Arg -> [X.Instr]
runAdd name ((C.AVar leftVar)) rightArg | name == leftVar =
    [ X.Addq (runArg rightArg) (X.AVar name) ]
runAdd name leftArg ((C.AVar rightVar)) | name == rightVar =
    [ X.Addq (runArg leftArg) (X.AVar name)  ]
runAdd name left right =
    [ X.Movq (runArg left) (X.AVar name),
      X.Addq (runArg right) (X.AVar name) ]

runArg :: C.Arg -> X.Arg
runArg (C.AVar n) = X.AVar n
runArg (C.AInt n) = X.AInt n
