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
            [ Movq (AVar "t0") (AInt 4),
              Addq (AVar "t0") (AInt 5),
              Movq (AVar "x0") (AVar "t0"),
              Subq (AVar "x0"),
              Movq (AReg RAX) (AVar "x0") ]
    ),
    ("let x = let y = (+ (- 1) 2) in (+ (- y) 3) in x",
        Program
            ["t1","t0","x0","y0"]
            [ Movq (AVar "t0") (AInt 1),
              Subq (AVar "t0"),
              Movq (AVar "y0") (AVar "t0"),
              Addq (AVar "y0") (AInt 2),
              Movq (AVar "t1") (AVar "y0"),
              Subq (AVar "t1"),
              Movq (AVar "x0") (AVar "t1"),
              Addq (AVar "x0") (AInt 3),
              Movq (AReg RAX) (AVar "x0") ]
    ),
    ("let a = 3 in let b = a in b",
        Program
            ["a0","b0"]
            [ Movq (AVar "a0") (AInt 3),
              Movq (AVar "b0") (AVar "a0"),
              Movq (AReg RAX) (AVar "b0")]
    )
    ]

tests :: TestTree
tests =
    testGroup "Selection" $
    map (\ (str, expected) ->
        let x = select str
        in testCase str $ x @?= expected)
        cases