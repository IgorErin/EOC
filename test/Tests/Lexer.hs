module Tests.Lexer (tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Lexer (alexScanTokens, Token (..))

cases :: [(String, [Token])]
cases = [
    ("34234", [TInt 34234]),
    ("- +", [TSub, TAdd]),
    ("( )", [TLParent, TRParent]), 
    ("read", [TRead])
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = alexScanTokens str in 
        testCase str $ x @?= expected) 
        cases 
