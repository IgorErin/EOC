module Tests.Parser (tests) where

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import Lexer (alexScanTokens)
import qualified Parser as P
import R1

cases :: [(String, Program)]
cases = [
    ("3", program $ int 3),
    ("(read)", program read_),
    ("(+ (read) 4)", program (add read_ $ int 4)),
    ("let x = 3 in x", program $ let_ "x" (int 3) (ident "x")),
    ("let x = 3 in let y = x in x", program $ let_ "x" (int 3) (let_ "y" (ident "x") (ident "x")))
    ]

run :: String -> Program
run = P.run . alexScanTokens

tests :: TestTree
tests =
    testGroup "Parsing" $
    map (\ (str, expected) ->
        let x = run str in
        testCase str $ x @?= expected
        )
        cases
