module Tests.AssignHome (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (assign)
import RV32

cases :: [(String, Program)]
cases = [ ("let x = 4 in x", Program {
            frameSize = 4,
            code = [Li T0 4,Sw T0 0 Sp,Lw A0 0 Sp]
            }),
          ("let x = (- (+ 4 5)) in x", Program 0 []),
          ("let a = 3 in let b = a in b", Program 0 [])
           ]

tests :: TestTree
tests =
    testGroup "Assign Home" $
    map (\ (str, expected) ->
        let x = assign str
        in testCase str $ x @?= expected)
        cases