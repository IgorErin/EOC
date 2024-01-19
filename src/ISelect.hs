module ISelect (run) where

import qualified C0 as C
import qualified RV

import Control.Monad.Writer
    ( MonadWriter(tell), execWriter, Writer )

run :: C.Program -> RV.Program
run (C.Program vars stmts) =
    let insts = execWriter $ mapM runStmt stmts
    in RV.Program vars insts

runStmt :: C.Stmt -> Writer [RV.Instr] ()
runStmt (C.SAssign name expr) =
    tell $ runAssign name expr
runStmt (C.SReturn (C.AVar ident)) =
    tell [ RV.Lw RV.A0 ident ]
runStmt (C.SReturn (C.AInt imm)) =
    tell [ RV.Li RV.A0 imm ]

runAssign :: C.Ident -> C.Expr -> [RV.Instr]
runAssign name (C.EArg (C.AInt imm)) =
    [ RV.Li RV.T0 imm,
      RV.Sw RV.T0 name ]
runAssign name (C.EArg (C.AVar ident)) =
    [ RV.Lw RV.T0  ident,
      RV.Sw RV.T0 name ]
runAssign name (C.ESub (C.AInt num)) =
    [ RV.Li RV.T0 (-num),
      RV.Sw RV.T0 name ]
runAssign name (C.ESub (C.AVar ident)) =
    [ RV.Lw RV.T0 ident,
      RV.Neg RV.T0 RV.T0,
      RV.Sw RV.T0 name ]
runAssign name (C.EAdd left right) =
    runAdd name left right
runAssign name C.ERead =
    [ RV.Jal "read_int",
      RV.Sw RV.A0 name ]

runAdd :: C.Ident -> C.Arg -> C.Arg -> [RV.Instr]
runAdd name (C.AVar leftIdent) (C.AVar rightIdent) =
    [ RV.Lw RV.T0 leftIdent,
      RV.Lw RV.T1 rightIdent,

      RV.Add RV.T3 RV.T0 RV.T1,

      RV.Sw RV.T3 name ]
runAdd name (C.AInt leftImm) (C.AInt rightImm) =
    [ RV.Li RV.T0 leftImm,
      RV.Addi RV.T0 RV.T0 rightImm,
      RV.Sw RV.T0 name ]
runAdd name (C.AInt num) (C.AVar ident) =
    [ RV.Lw RV.T0 ident,
      RV.Addi RV.T0 RV.T0 num,
      RV.Sw RV.T0 name ]
runAdd name (C.AVar ident) (C.AInt num) =
    [ RV.Lw RV.T0 ident,
      RV.Addi RV.T0 RV.T0 num,
      RV.Sw RV.T0 name ]
