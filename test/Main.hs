module Main (main) where

import Test.Tasty

import qualified Tests.Lexer as L (tests)
import qualified Tests.Parser as P (tests)
import qualified Tests.Pareval as PE (tests)
import qualified Tests.Inter as I (tests)
import qualified Tests.Uniquify as U (tests)
import qualified Tests.Flatten as F (tests)
import qualified Tests.ISelect as IS (tests)
import qualified Tests.AssignHome as AH (tests)
import qualified Tests.MemPatch as MP (tests)

tests :: TestTree
tests = testGroup "Main" [
    L.tests,
    P.tests,
    PE.tests,
    I.tests,
    U.tests,
    F.tests,
    IS.tests,
    AH.tests,
    MP.tests ]

main :: IO ()
main = defaultMain tests