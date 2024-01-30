module Run (checkCode) where

import System.Process.Typed
import System.FilePath ((</>), (<.>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as TIO

import Fmt ( (+||), (||+) )

import Control.Monad.Cont (ContT (..))

import Lang (finale)
import UnliftIO (withTempFile, withTempDirectory, MonadIO (liftIO), Handle)

checkCode :: FilePath  -> IO ExitCode
checkCode src = runContT (runDir src) return

runDir :: FilePath -> ContT ExitCode IO ExitCode
runDir srcPath = do
    dir <- ContT $ withTempDirectory "." "compiled."

    runFile dir srcPath

-- ingore handler
withTempFile' :: FilePath -> String -> ContT r IO FilePath
withTempFile' f s =
    let base :: (FilePath -> Handle -> IO a) -> IO a
        base = withTempFile f s

        step :: (FilePath -> IO a) -> IO a
        step k = base $ \fp _ -> k fp
    in
    ContT step

runFile :: FilePath -> FilePath -> ContT ExitCode IO ExitCode
runFile tempDir srcPath = do
    let getTempFile = withTempFile' tempDir

    exec <- getTempFile "exec."
    liftIO $ gccFileTo srcPath exec

    liftIO $ run exec

runtimePath :: FilePath
runtimePath = "runtime" </> "runtime" <.> "c"

gccFileTo :: FilePath -> FilePath -> IO ()
gccFileTo srcPath destPath = do
    let output = "-o"+||destPath||+""

    runProcess_ $ proc "gcc" [srcPath, runtimePath, output]

run :: FilePath -> IO ExitCode
run = runProcess . flip proc []

-- asm      Golden/Print/testn.s
-- runtime  runtime/runtime.c