module Main (main) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Lang as L
import qualified Golden as G
import qualified Run as R

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [
  G.show_ L.lexing "Lexing",
  G.show_ L.parsing "Parsing",
  G.show_ L.unique "Unique",
  G.show_ L.flatten "Flatten",
  G.show_ L.select "Select",

  G.text_ L.assign "Assign",
  G.text_ L.mempatch "Mempatch",

  G.textAsm L.finale "Asm" ]

main :: IO ()
main = defaultMain =<< tests