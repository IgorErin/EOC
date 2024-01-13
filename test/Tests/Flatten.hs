module Tests.Flatten (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (flatten)
import C0

cases :: [(String, Program)]
cases = [
    ("let x = (- (+ 4 5)) in x",
      Program
        ["t0","x0"]
        [SAssign "t0" (EAdd (AInt 4) (AInt 5)),
         SAssign "x0" (ESub (AVar "t0")),
         SReturn (AVar "x0")]),
    ("let x = let y = (+ (- 1) 2) in (+ (- y) 3) in x",
      Program
        ["t1","t0","x0","y0"]
        [SAssign "t0" (ESub (AInt 1)),
         SAssign "y0" (EAdd (AVar "t0") (AInt 2)),
         SAssign "t1" (ESub (AVar "y0")),
         SAssign "x0" (EAdd (AVar "t1") (AInt 3)),
         SReturn (AVar "x0")]),
    ("let a = 3 in let b = a in b",
      Program
        ["a0","b0"]
        [SAssign "a0" (EArg (AInt 3)),
         SAssign "b0" (EArg (AVar "a0")),
         SReturn (AVar "b0")])
    ]

tests :: TestTree
tests =
    testGroup "Flatten" $
    map (\ (str, expected) ->
        let x = flatten str
        in testCase str $ x @?= expected)
        cases