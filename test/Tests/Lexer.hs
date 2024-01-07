module Tests.Lexer (tests) where 

import Test.Tasty ( testGroup, TestTree ) 
import Test.Tasty.HUnit ((@?=), testCase)

import Lexer (alexScanTokens, Token (..))

cases :: [(String, [Token])]
cases = [
    ("34234", [TInt 34234]),
    ("- +", [TSub, TAdd]),
    ("( )", [TLParent, TRParent]), 
    ("read", [TRead]),
    ("let x = 3 in x", [TLet,TIdent "x",TBind,TInt 3,TIn,TIdent "x"])
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = alexScanTokens str in 
        testCase str $ x @?= expected) 
        cases 
