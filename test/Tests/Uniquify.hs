module Tests.Uniquify (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (unique)
import Ast

cases :: [(String, Expr)]
cases = [
    ("let x = 3 in x",
      let_ "x0" (int 3) (ident "x0")),
    ("let x = 3 in let x = x in x",
     let_ "x0" (int 3) (let_ "x1" (ident "x0") (ident "x1"))),
    ("let x = 4 in let x = let x = x in x in x",
        let_ "x0" (int 4) (let_ "x1" (let_ "x2" (ident "x0") (ident "x2")) (ident "x1"))),
    ("let x = let y = 4 in y in x", let_ "x0" (let_ "y0" (int 4) (ident "y0")) (ident "x0"))]

tests :: TestTree
tests =
    testGroup "Uniauify" $
    map (\ (str, expected) ->
        let x = unique str
            expected' = program expected
        in testCase str $ x @?=  expected')
        cases
