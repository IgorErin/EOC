module Lang
    (initState,

    lexing,
    parsing,
    -- run, TODO add interpreter for property based tests
    unique,
    flatten,
    select,
    assign,
    mempatch,
    finale)
    where

import qualified ParseTree as PT
import qualified R1 as R
import qualified C0 as C
import qualified X86V
import qualified X86

import qualified Parser as P
import qualified Lexer as L
import qualified Uniquify as U
import qualified Flatten as F
import qualified ISelect as IS
import qualified AssignHome as AH
import qualified MemPatch as MP
import qualified PreludeConclude as PC
import qualified Print as Pr

import qualified Ident(Seed, initSeed)

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

initState :: Ident.Seed
initState = Ident.initSeed

lexing :: ByteString -> [L.Token]
lexing = L.alexScanTokens

parsing :: ByteString -> PT.Program
parsing = P.run . lexing

unique :: ByteString -> R.Program
unique = U.run . parsing

flatten :: ByteString -> C.Program
flatten = F.run . unique

select :: ByteString -> X86V.Program
select = IS.run . flatten

----------- Assign ---------------

assign' :: ByteString -> X86.Program
assign' = AH.run . select

assign :: ByteString -> X86.Program
assign = assign'

----------- Patch ---------------

mempatch' :: ByteString -> X86.Program
mempatch' = MP.run . assign'

mempatch :: ByteString -> X86.Program
mempatch = mempatch'

----------- Finale ---------------

finale :: ByteString -> Text
finale = Pr.run . PC.run .mempatch'
