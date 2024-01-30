{
{-# LANGUAGE FieldSelectors #-}

module Lexer(alexScanTokens, Token (..)) where

import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TL ()

import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text.Encoding         as T
import Data.ByteString.Lazy.Char8 as BSC

import Data.Maybe (fromJust)
}

%wrapper "basic-bytestring"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  $digit+               { int_ }

  "-"                   { \_ -> TSub }
  "+"                   { \_ -> TAdd }

  "read"                { \_ -> TRead }

  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent}

  "let"                 { \_ -> TLet }
  "in"                  { \_ -> TIn }
  "="                   { \_ -> TBind }

  @ident                { ident }

{
-- Each action has type :: String -> Token

-- The token type:
ident :: BL.ByteString -> Token
ident = TIdent . T.decodeUtf8 . B.concat . BL.toChunks

int_ :: BL.ByteString -> Token
int_ = TInt . fst . fromJust . BSC.readInt

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