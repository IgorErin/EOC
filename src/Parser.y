{
module Parser where

import qualified Lexer as L
import qualified ParseTree as PT
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

   let      { L.TLet }
   in       { L.TIn }
   '='      { L.TBind }

   ident    { L.TIdent $$ }

%%

Program :: { PT.Program }
Program : Expr                      { PT.program $1 }

Expr :: { PT.Expr }
Expr
    : int                           { PT.int $1 }
    | '(' read ')'                  { PT.read_ }
    | '(' '-' Expr ')'              { PT.sub $3 }
    | '(' '+' Expr Expr ')'         { PT.add $3 $4 }
    | let ident '=' Expr in Expr    { PT.let_ $2 $4 $6 }
    | ident                         { PT.ident $1 }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
