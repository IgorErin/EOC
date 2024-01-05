module Main (main) where 

import Test.Tasty

import qualified Tests.Lexer as L (tests)

tests :: TestTree
tests = testGroup "Main" [L.tests]

main :: IO ()
main = defaultMain tests