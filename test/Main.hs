module Main (main) where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import System.FilePath (takeBaseName, (</>), (<.>))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import Data.String (fromString)
import Data.Text.Lazy ( Text )
import Data.Text.Lazy.Encoding as TLE (encodeUtf8)

import Text.Show.Pretty (ppShow)

import qualified Lang as L
import qualified Unit.Inter as Inter (tests)

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [
  -- Golden tests
  goldenShow L.lexing "Lexing",
  goldenShow L.parsing "Parsing",
  goldenShow L.partial "Partial",
  goldenShow L.unique "Unique",
  goldenShow L.flatten "Flatten",
  goldenShow L.select "Select",
  goldenShow L.assign "Assign",

  goldenText L.toString "Print",

  -- Unit Tests
  pure Inter.tests
  ]

main :: IO ()
main = defaultMain =<< tests

goldenShow :: Show a => (LBS.ByteString -> a) -> String -> IO TestTree
goldenShow testFun = golden (BSB.toLazyByteString . fromString . ppShow . testFun)

goldenText :: (LBS.ByteString -> Text) -> String -> IO TestTree
goldenText testFun = golden (TLE.encodeUtf8 . testFun)

golden :: (LBS.ByteString -> LBS.ByteString) -> String -> IO TestTree
golden testFun name = do
  srcFiles <- findByExtension [".np"] "examples"
  return $ testGroup ("Golden " ++ name)
    [ goldenVsStringDiff
        (takeBaseName srcFile)
        (\ref new -> ["diff", "-u", ref, new])
        (resultDir </> resultFile)
        (testFun <$> srcCode)
    | srcFile <- srcFiles
    , let resultDir = "test" </> "Golden" </> takeBaseName srcFile
    , let resultFile = name <.> "golden"
    , let srcCode = LBS.readFile srcFile
    ]

