{
module Parser where

import qualified Lexer as L
import qualified R1 as R
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

Program :: { R.Program }
Program : Expr              { R.program $1 }

Expr :: { R.Expr }
Expr
    : int                           { R.int $1 }
    | '(' read ')'                  { R.read_ }
    | '(' '-' Expr ')'              { R.sub $3 }
    | '(' '+' Expr Expr ')'         { R.add $3 $4 }
    | let ident '=' Expr in Expr    { R.let_ $2 $4 $6 }
    | ident                         { R.ident $1 }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
