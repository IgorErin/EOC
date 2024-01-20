{
module Lexer(alexScanTokens, Token (..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS (readInt)

}

%wrapper "basic-bytestring"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  $digit+               { \s -> TInt undefined }

  "-"                   { \_ -> TSub }
  "+"                   { \_ -> TAdd }

  "read"                { \_ -> TRead }

  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent}

  "let"                 { \_ -> TLet }
  "in"                  { \_ -> TIn }
  "="                   { \_ -> TBind }

  @ident                { \ x -> TIdent $ undefined }

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
  | TIdent Text
  deriving (Eq, Show)
}