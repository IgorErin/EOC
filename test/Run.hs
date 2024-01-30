module Run (checkCode) where

import System.Process.Typed
import System.FilePath ((</>), (<.>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as TIO

import Fmt ( (+||), (||+) )

import Control.Monad.Cont (ContT (..))

import Lang (finale)
import UnliftIO (withTempFile, withTempDirectory, MonadIO (liftIO), Handle)

-- runFile :: FilePath -> IO ()
-- runFile fpath =

--     undefined

checkCode :: FilePath  -> IO ExitCode
checkCode src = runContT (runDir src) return

runDir :: FilePath -> ContT ExitCode IO ExitCode
runDir srcPath = do
    dir <- ContT $ withTempDirectory "." "compiled."

    runFile dir srcPath

runFile :: FilePath -> FilePath -> ContT ExitCode IO ExitCode
runFile tempDir srcPath = do
    let exec = dest tempDir

    liftIO $ gccFileTo exec srcPath
    liftIO $ run exec

dest :: FilePath -> FilePath
dest dir = dir </> "a.out"

runtimePath :: FilePath
runtimePath =  "." </> "runtime" </> "runtime" <.> "c"

gccFileTo :: FilePath -> FilePath -> IO ()
gccFileTo tempDir srcPath  = do
    let output = "-o" ++ tempDir

    runProcess_ $ proc "gcc" [srcPath, runtimePath, output]

run :: FilePath -> IO ExitCode
run = runProcess . flip proc []