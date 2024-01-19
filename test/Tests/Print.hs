module Tests.Print (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (toString)

cases :: [(String, String)]
cases = []

tests :: TestTree
tests = undefined
    -- testGroup "Print" $
    -- map (\ (str, expected) ->
    --     let x = toString str
    --     in testCase str $ x @?= expected)
    --     cases