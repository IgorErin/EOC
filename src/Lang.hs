module Lang
    (lexing,
    parsing,
    partial,
    run,
    unique,
    flatten,
    select,
    assign,
    toString)
    where

import qualified R1 as R
import qualified C0 as C
import qualified RV
import qualified RV32

import qualified Parser as P
import qualified Pareval as PE
import qualified Lexer as L
import qualified Inter as I
import qualified Uniquify as U
import qualified Flatten as F
import qualified ISelect as IS
import qualified AssignHome as AH
import qualified Print as Pr

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

select :: String -> RV.Program
select = IS.run . flatten

assign :: String -> RV32.Program
assign = AH.run . select

toString :: String -> String
toString = Pr.run . assign
