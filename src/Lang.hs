module Lang
    (lexing,
    parsing,
    partial,
    run,
    unique,
    flatten)
    where

import qualified R1 as R
import qualified C0 as C

import qualified Parser as P
import qualified Pareval as PE
import qualified Lexer as L
import qualified Inter as I
import qualified Uniquify as U
import qualified Flatten as F

lexing :: String -> [L.Token]
lexing = L.alexScanTokens

parsing :: String -> R.Program
parsing = P.run . lexing

partial :: String -> R.Program
partial = PE.run . parsing

run :: [Int] -> String -> Int
run args = I.run args . parsing

unique :: String -> R.Program
unique = U.run . parsing

flatten :: String -> C.Program
flatten = F.run . unique
