module Tests.Parser (tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Lexer (alexScanTokens)
import qualified Parser as P
import Ast 

cases :: [(String, Program)]
cases = [
    ("3", program $ int 3),
    ("(read)", program (read_)),
    ("(+ (read) 4)", program (add read_ $ int 4))]

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
