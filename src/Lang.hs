module Lang
    (lexing,
    parsing,
    partial,
    run,
    unique,
    flatten,
    select,
    assign,
    mempatch,
    toString)
    where

import qualified R1 as R
import qualified C0 as C
import qualified X86V as XV
import qualified X86 as X

import qualified Parser as P
import qualified Pareval as PE
import qualified Lexer as L
import qualified Inter as I
import qualified Uniquify as U
import qualified Flatten as F
import qualified ISelect as IS
import qualified AssignHome as AH
import qualified MemPatch as MP
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

select :: String -> XV.Program
select = IS.run . flatten

assign :: String -> X.Program
assign = AH.run . select

mempatch :: String -> X.Program
mempatch = MP.run . assign

toString :: String -> String
toString = Pr.run . mempatch
