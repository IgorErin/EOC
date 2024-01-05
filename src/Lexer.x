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

  "read"                { \_ -> TRead } -- TODO

  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent}

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
  deriving (Eq, Show)
}