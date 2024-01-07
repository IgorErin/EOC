module Lang
    (lexing,
    parsing,
    partial,
    run)
    where

import qualified Parser as P
import qualified Pareval as PE
import qualified Lexer as L
import qualified Ast as A
import qualified Inter as I

lexing :: String -> [L.Token]
lexing = L.alexScanTokens

parsing :: String -> A.Program
parsing = P.run . lexing

partial :: String -> A.Program
partial = PE.run . parsing

run :: [Int] -> String -> Int
run args = I.run args . parsing

