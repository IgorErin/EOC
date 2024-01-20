module Main (main) where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Golden (goldenVsString, findByExtension)

import System.FilePath (takeBaseName, replaceExtension)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import Data.String

import Text.Show.Pretty (ppShow)

import qualified Lang as L

import qualified Unit.Inter as Inter (tests)

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [
  -- Golden tests
  goldenTests L.lexing "Lexing",
  goldenTests L.parsing "Parsing",
  goldenTests L.partial "Partial",
  goldenTests L.unique "Unique",
  goldenTests L.flatten "Flatten",
  goldenTests L.select "Select",
  goldenTests L.assign "Assign",

  -- Unit Tests
  pure Inter.tests
  ]

main :: IO ()
main = defaultMain =<< tests

goldenTests :: Show a => (LBS.ByteString -> a) -> String -> IO TestTree
goldenTests testFun name = do
  srcFiles <- findByExtension [".np"] $ "test/Golden/"
  return $ testGroup ("Golden " ++ name)
    [ goldenVsString
        (takeBaseName srcFile)
        jsonFile
        (BSB.toLazyByteString <$> fromString <$> ppShow <$> result)
    | srcFile <- srcFiles
    , let jsonFile = replaceExtension srcFile $ "." ++ name ++ ".golden"
    , let srcCode = LBS.readFile srcFile
    , let result = testFun <$> srcCode
    ]
