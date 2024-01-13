module Tests.ISelect (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (select)
import X86V

cases :: [(String, Program)]
cases = [
    ("let x = (- (+ 4 5)) in x",
        Program
        ["t0","x0"]
        [ Movq (AInt 4) (AVar "t0"),
          Addq (AInt 5) (AVar "t0"),
          Movq (AVar "t0") (AVar "x0"),
          Subq (AVar "x0"),
          Movq (AVar "x0") (AReg RAX) ]
    ),
    ("let x = let y = (+ (- 1) 2) in (+ (- y) 3) in x",
        Program
        ["t1","t0","x0","y0"]
        [ Movq (AInt 1) (AVar "t0"),
          Subq (AVar "t0"),
          Movq (AVar "t0") (AVar "y0"),
          Addq (AInt 2) (AVar "y0"),
          Movq (AVar "y0") (AVar "t1"),
          Subq (AVar "t1"),
          Movq (AVar "t1") (AVar "x0"),
          Addq (AInt 3) (AVar "x0"),
          Movq (AVar "x0") (AReg RAX) ]
    ),
    ("let a = 3 in let b = a in b",
        Program
        ["a0","b0"]
        [ Movq (AInt 3) (AVar "a0"),
          Movq (AVar "a0") (AVar "b0"),
          Movq (AVar "b0") (AReg RAX) ]
    )]

tests :: TestTree
tests =
    testGroup "Selection" $
    map (\ (str, expected) ->
        let x = select str
        in testCase str $ x @?= expected)
        cases