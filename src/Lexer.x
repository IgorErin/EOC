{
    module Lexer(alexScanTokens, Token (..)) where 
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  $digit+               { \s -> TInt (read s) }

  "-"                   { \_ -> TSub }
  "+"                   { \_ -> TAdd }

  "read"                { \_ -> TRead }

  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent}

  "let"                 { \_ -> TLet }
  "in"                  { \_ -> TIn }
  "="                   { \_ -> TBind }

  @ident                { \ x -> TIdent x }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = 
  TInt Int 
  | TSub 
  | TAdd 
  | TRead 

  | TLParent 
  | TRParent

  | TLet 
  | TIn 
  | TBind
  | TIdent String
  deriving (Eq, Show)
}