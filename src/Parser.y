{
module Parser where 

import qualified Lexer as L 
import qualified Ast as A   
}

%name run 
%tokentype  { L.Token }
%error      { parseError }

%token
   int      { L.TInt $$ }
   read     { L.TRead } 
   '-'      { L.TSub }
   '+'      { L.TAdd }

   '('      { L.TLParent }
   ')'      { L.TRParent }
   
%%

Program :: { A.Program }
Program : Expr              { A.program $1 }

Expr :: { A.Expr }
Expr 
    : int                   { A.int $1 }
    | '(' read ')'          { A.read_ }
    | '(' '-' Expr ')'      { A.sub $3 }
    | '(' '+' Expr Expr ')' { A.add $3 $4 }    

{
parseError :: [L.Token] -> a 
parseError _ = error "Parse error"
}
