{-#
LANGUAGE
  TemplateHaskell,
  InstanceSigs
#-}

module Golden (show_, text_, textAsm) where

import Test.Tasty (testGroup, TestTree )
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import System.FilePath (takeBaseName, (</>), (<.>))

import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import Data.Text.Lazy             as TL (Text, pack)
import Data.Text.Lazy.Encoding    as TL (encodeUtf8)

import Text.Pretty.Simple (pShowNoColor)

import Control.Lens hiding ((<.>))

import qualified Examples (paths)

--------------------- Meta data ---------------------------

data TExt = Asm | Golden

instance Show TExt where
  show :: TExt -> String
  show Asm = "s"
  show Golden = "golden"

type TestFun = (BL.ByteString -> BL.ByteString)

data TestData = MkTestData {
  _tname :: String,
  _text  :: TExt,
  _tfun  :: TestFun
}

$(makeLenses ''TestData)

---------------------- Methods ------------------------------

show_ :: Show a => (BL.ByteString -> a) -> String -> IO TestTree
show_ testFun name =
  let testFun' = TLE.encodeUtf8 . pShowNoColor . testFun
      td = mkTestData name Golden testFun'
  in  mkTests td <$> src

text_ :: (BL.ByteString -> Text) -> String -> IO TestTree
text_ testFun name =
  let testFun' = TLE.encodeUtf8 . testFun
      td = mkTestData name Golden testFun'
  in mkTests td <$> src

textAsm :: (BL.ByteString -> Text) -> String -> IO TestTree
textAsm testFun name =
  let testFun' = TLE.encodeUtf8 . testFun
      td = mkTestData name Asm testFun'
  in mkTests td <$> src

---------------------- Dirs ----------------------------

src :: IO [FilePath]
src = findByExtension [".np"] "examples"

asm :: IO [FilePath]
asm = findByExtension [".s"] "test/Golden/Asm"


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

        result = view tfun td <$> BL.readFile tsrc
    in golden td resultPath result

mkResultPath :: TestData -> FilePath -> FilePath
mkResultPath td tsrc =
  let bn = takeBaseName tsrc
      name = view tname td
      ext = show $ view text td
  in "test" </> "Golden" </> bn </> name <.> ext

golden :: TestData -> FilePath -> IO BL.ByteString -> TestTree
golden td resultPath result =
  let diff :: FilePath -> FilePath -> [FilePath]
      diff ref new = ["diff", "-u", ref, new]

  in goldenVsStringDiff (view tname td) diff resultPath result
