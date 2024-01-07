module Main (main) where

import Test.Tasty

import qualified Tests.Lexer as L (tests)
import qualified Tests.Parser as P (tests)
import qualified Tests.Pareval as PE (tests)
import qualified Tests.Inter as I (tests)

tests :: TestTree
tests = testGroup "Main" [L.tests, P.tests, PE.tests, I.tests]

main :: IO ()
main = defaultMain tests