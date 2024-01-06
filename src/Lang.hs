module Lang (lexing, parsing, partial) where 

import qualified Parser as P
import qualified Pareval as PE 
import qualified Lexer as L
import qualified Ast as A 

lexing :: String -> [L.Token]
lexing = L.alexScanTokens

parsing :: String -> A.Program 
parsing = P.run . lexing

partial :: String -> A.Program 
partial = PE.run . parsing 

