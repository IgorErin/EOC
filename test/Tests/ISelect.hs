module Tests.ISelect (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (select)
import RV

cases :: [(String, Program)]
cases = [ ("let x = 4 in x", Program ["x0"] [Li T0 4,Sw T0 "x0",Lw A0 "x0"]),
          ("let x = (- (+ 4 5)) in x",
           Program
            ["t0","x0"]
            [ Li T0 4,
              Addi T0 T0 5,
              Sw T0 "t0",
              Lw T0 "t0",
              Neg T0 T0,
              Sw T0 "x0",
              Lw A0 "x0" ]
           ),
          ("let a = 3 in let b = a in b",
           Program
               ["a0","b0"]
               [ Li T0 3,
                 Sw T0 "a0",
                 Lw T0 "a0",
                 Sw T0 "b0",
                 Lw A0 "b0" ]
          ) ]

tests :: TestTree
tests =
    testGroup "Selection" $
    map (\ (str, expected) ->
        let x = select str
        in testCase str $ x @?= expected)
        cases