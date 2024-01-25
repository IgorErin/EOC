{-# LANGUAGE NamedFieldPuns #-}

module ISelect (run) where

import qualified C0 as C
import X86V

import Ident (Ident)
import Regs

import Control.Monad.Writer (MonadWriter(tell), execWriter, Writer )

run :: C.Program -> X86V.Program
run (C.Program { C.body, C.result }) =
    let body' = execWriter $ do
            mapM_ runStmt body
            runReturn result
    in X86V.Program body'

runReturn :: C.Arg -> Writer [X86V.Instr] ()
runReturn (C.AVar ident) =
    tell [ Movq (AVar ident) (AReg Rax) ]
runReturn (C.AInt imm) =
    tell [ Movq (AImm imm) $ AReg Rax ]

runStmt :: C.Stmt -> Writer [X86V.Instr] ()
runStmt (C.SAssign name expr) =
    tell $ runAssign name expr

runAssign :: Ident -> C.Expr -> [X86V.Instr]
runAssign name (C.EArg (C.AInt imm)) =
    [ Movq (AImm imm) (AVar name) ]
runAssign name (C.EArg (C.AVar ident)) =
    [ Movq (AVar ident) (AVar name) ]
runAssign name (C.ESub (C.AInt num)) =
    [ Movq (AImm (-num)) (AVar name) ]
runAssign name (C.ESub (C.AVar ident)) =
    [ Movq (AVar ident) (AVar ident),
      Negq (AVar name) ]
runAssign name (C.EAdd left right) =
    runAdd name left right
runAssign name C.ERead =
    [ Callq "read_int",
      Movq (AReg Rax) (AVar name) ]

straightAddArgs :: Arg -> Arg -> Arg -> [Instr]
straightAddArgs dest left right =
    [ Movq left dest,
      Addq right dest ]

runAdd :: Ident -> C.Arg -> C.Arg -> [Instr]
runAdd name (C.AVar leftIdent) (C.AVar rightIdent)
    | name == leftIdent = [ Addq (AVar rightIdent) (AVar name) ]
    | name == rightIdent = [ Addq (AVar leftIdent) (AVar name) ]
    | otherwise = straightAddArgs (AVar name) (AVar leftIdent) (AVar rightIdent)
runAdd name (C.AInt num) (C.AVar ident)
    | name == ident = [ Addq (AImm num) $ AVar name ]
    | otherwise = straightAddArgs (AVar name) (AImm num) (AVar ident)
runAdd name (C.AVar ident) (C.AInt num)
    | name == ident = [ Addq (AImm num) $ AVar name]
    | otherwise = straightAddArgs (AVar name) (AVar ident) (AImm num)
runAdd name (C.AInt leftNum) (C.AInt rightNum) =
    straightAddArgs (AVar name) (AImm leftNum) (AImm rightNum)
