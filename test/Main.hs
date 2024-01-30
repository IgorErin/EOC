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

  G.show_ L.assign "Assign",
  G.show_ L.mempatch "Mempatch",

  G.textAsm L.finale "Asm",
  G.fromFileShow R.checkCode "Run" ]

main :: IO ()
main = defaultMain =<< tests