module Tests.AssignHome (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (assign)
import X86

cases :: [(String, Program)]
cases = [
    ("let x = (- (+ 4 5)) in x",
    Program 8
    [ Movq (AInt 4) (ADeref 0 RBP),
      Addq (AInt 5) (ADeref 0 RBP),
      Movq (ADeref 0 RBP) (ADeref 4 RBP),
      Subq (ADeref 4 RBP),
      Movq (ADeref 4 RBP) (AReg RAX) ]
    ),
    ("let x = let y = (+ (- 1) 2) in (+ (- y) 3) in x",
    Program 16
    [ Movq (AInt 1) (ADeref 0 RBP),
      Subq (ADeref 0 RBP),
      Movq (ADeref 0 RBP) (ADeref 4 RBP),
      Addq (AInt 2) (ADeref 4 RBP),
      Movq (ADeref 4 RBP) (ADeref 8 RBP),
      Subq (ADeref 8 RBP),
      Movq (ADeref 8 RBP) (ADeref 12 RBP),
      Addq (AInt 3) (ADeref 12 RBP),
      Movq (ADeref 12 RBP) (AReg RAX) ]
    ),
    ("let a = 3 in let b = a in b",
    Program 8
    [ Movq (AInt 3) (ADeref 0 RBP),
      Movq (ADeref 0 RBP) (ADeref 4 RBP),
      Movq (ADeref 4 RBP) (AReg RAX) ]
    )]

tests :: TestTree
tests =
    testGroup "Assign Home" $
    map (\ (str, expected) ->
        let x = assign str
        in testCase str $ x @?= expected)
        cases