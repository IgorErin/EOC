module Tests.Inter where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (run)
import R1

cases :: [(String, [Int], Int)]
cases = [
    ("4", [], 4),
    ("5425", [], 5425),
    ("let x = 4 in (+ 2 x)", [], 6),
    ("let x = 4 in (- x)", [], -4),
    ("(+ 3 (- 2))", [], 1)]

tests :: TestTree
tests =
    testGroup "Inter" $
    map (\ (str, input, expected) ->
        let x = run input str in
        testCase str $ x @?= expected)
        cases
