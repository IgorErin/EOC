{-#
LANGUAGE
  TemplateHaskell,
  InstanceSigs
#-}

module Golden (show_, text_, textAsm, fromFileShow) where

import Test.Tasty (testGroup, TestTree )
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import System.FilePath (replaceBaseName, replaceExtension)

import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import Data.Text.Lazy             as TL (Text)

import Text.Pretty.Simple (pShowNoColor)

import Control.Lens hiding ((<.>))

--------------------- Meta data ---------------------------

data TExt = Asm | Golden

instance Show TExt where
  show :: TExt -> String
  show Asm = "s"
  show Golden = "golden"

type TestFun = (FilePath -> IO BL.ByteString)

data TestData = MkTestData {
  _tname :: String,
  _text  :: TExt,
  _tfun  :: TestFun
}

$(makeLenses ''TestData)

----------------------- read helpers ------------------------

showToBS :: Show a =>  a -> BL.ByteString
showToBS = TLE.encodeUtf8 . pShowNoColor

textToBS :: Text -> BL.ByteString
textToBS = TLE.encodeUtf8

---------------------- Methods ------------------------------

show_ :: Show a => (BL.ByteString -> a) -> String -> IO TestTree
show_ testFun name =
  let testFun' x = showToBS . testFun <$> BL.readFile x
      td = mkTestData name Golden testFun'
  in  mkTests td <$> src

text_ :: (BL.ByteString -> Text) -> String -> IO TestTree
text_ testFun name =
  let testFun' x = textToBS .  testFun <$> BL.readFile x
      td = mkTestData name Golden testFun'
  in mkTests td <$> src

textAsm :: (BL.ByteString -> Text) -> String -> IO TestTree
textAsm testFun name =
  let testFun' x = textToBS . testFun <$> BL.readFile x
      td = mkTestData name Asm testFun'
  in mkTests td <$> src

fromFileShow :: (Show a) => (FilePath -> IO a) -> String -> IO TestTree
fromFileShow testFun name =
  let testFun' x = showToBS <$> testFun x
      td = mkTestData name Golden testFun'
  in mkTests td <$> asm

---------------------- Dirs ----------------------------

src :: IO [FilePath]
src = findByExtension [".np"] "test/Golden"

asm :: IO [FilePath]
asm = findByExtension [".s"] "test/Golden/"

------------------ Helpers --------------------------------

mkTests :: TestData -> [FilePath]  -> TestTree
mkTests td paths=
  let name = "Golden" ++ view tname td
  in testGroup name $ createGolden td <$> paths

mkTestData :: String -> TExt -> TestFun -> TestData
mkTestData = MkTestData

createGolden :: TestData -> FilePath -> TestTree
createGolden td tsrc =
    let resultPath = mkResultPath td tsrc
        result = view tfun td tsrc
    in golden td resultPath result

mkResultPath :: TestData -> FilePath -> FilePath
mkResultPath td tsrc =
  let name = view tname td
      ext = show $ view text td
  in flip replaceExtension ext $ replaceBaseName tsrc name

golden :: TestData -> FilePath -> IO BL.ByteString -> TestTree
golden td resultPath result =
  let diff :: FilePath -> FilePath -> [FilePath]
      diff ref new = ["diff", "-u", ref, new]

  in goldenVsStringDiff (view tname td) diff resultPath result
