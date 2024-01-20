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

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

lexing :: ByteString -> [L.Token]
lexing = L.alexScanTokens

parsing :: ByteString -> R.Program
parsing = P.run . lexing

partial :: ByteString -> R.Program
partial = PE.run . parsing

run :: [Int] -> ByteString -> Int
run args = I.run args . parsing

unique :: ByteString -> R.Program
unique = U.run . parsing

flatten :: ByteString -> C.Program
flatten = F.run . unique

select :: ByteString -> RV.Program
select = IS.run . flatten

assign :: ByteString -> RV32.Program
assign = AH.run . select

toString :: ByteString -> Text
toString = Pr.run . assign
