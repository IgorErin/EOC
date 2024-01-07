module Tests.Pareval (tests) where 

import Test.Tasty ( TestTree, testGroup ) 
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (partial)
import Ast 

cases :: [(String, Program)]
cases = [
    ("3", program $ int 3),
    ("(read)", program read_),
    ("(+ (read) 4)", program (add read_ $ int 4)),
    ("(+ 4 4)", program $ int 8),
    ("(+ (read) (+ (read) (+ 4 (- 4))))", program $ add read_ read_),
    ("(+ (read) 4)", program (add read_ $ int 4)),
    ("(+ 1 (+ (read) 1))", program (add read_ (int 2))) ]

tests :: TestTree
tests = 
    testGroup "Partial" $
    map (\ (str, expected) -> 
        let x = partial str in 
        testCase str $ x @?= expected) 
        cases 
