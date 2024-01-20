module Ident (Ident, newIdent) where

import Data.Text (Text, pack)

type Ident = Text

newIdent :: Int -> Text -> Ident
newIdent count name = name <> pack ( show count)