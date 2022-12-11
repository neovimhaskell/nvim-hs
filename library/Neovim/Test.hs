{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Neovim.Test
Description :  Testing functions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC
-}
module Neovim.Test (
    runInEmbeddedNeovim,
    runInEmbeddedNeovim',
    -- deprecated
    testWithEmbeddedNeovim,
    Seconds (..),
) where

import Neovim (Neovim, def, putDoc, void)
import Neovim.API.Text (nvim_command, vim_command)
import qualified Neovim.Context.Internal as Internal
import Neovim.RPC.Common (RPCConfig, newRPCConfig)
import Neovim.RPC.EventHandler (runEventHandler)
import Neovim.RPC.SocketReader (runSocketReader)

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (Default)
import Data.Text (pack)
import GHC.IO.Exception (ioe_filename)
import Path (File, Path, toFilePath)
import Prettyprinter (annotate, vsep)
import Prettyprinter.Render.Terminal (Color (..), color)
import System.Process.Typed (
    ExitCode (ExitFailure, ExitSuccess),
    Process,
    createPipe,
    getExitCode,
    getStdin,
    getStdout,
    proc,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    waitExitCode,
 )
import UnliftIO (
    Handle,
    IOException,
    async,
    atomically,
    cancel,
    catch,
    putTMVar,
    throwIO,
    timeout,
 )
import UnliftIO.Concurrent (threadDelay)

-- | Type synonym for 'Word'.
newtype Seconds = Seconds Word
    deriving (Show)

microSeconds :: Integral i => Seconds -> i
microSeconds (Seconds s) = fromIntegral s * 1000 * 1000

newtype TestConfiguration = TestConfiguration
    { cancelAfter :: Seconds
    }
    deriving (Show)

instance Default TestConfiguration where
    def =
        TestConfiguration
            { cancelAfter = Seconds 2
            }

{- | Run a neovim process with @-n --clean --embed@ and execute the
 given action that will have access to the started instance.

The 'TestConfiguration' contains sensible defaults.

'env' is the state of your function that you want to test.
-}
runInEmbeddedNeovim :: TestConfiguration -> env -> Neovim env a -> IO ()
runInEmbeddedNeovim TestConfiguration{..} env (Internal.Neovim action) =
    runTest `catch` catchIfNvimIsNotOnPath
  where
    runTest = do
        (nvimProcess, cfg, cleanUp) <- startEmbeddedNvim cancelAfter

        let actionCfg = Internal.retypeConfig env cfg

        result <- timeout (microSeconds cancelAfter) $ do
            runReaderT (runResourceT action) actionCfg

        -- vim_command isn't asynchronous, so we need to avoid waiting for the
        -- result of the operation since neovim cannot send a result if it
        -- has quit.
        let Internal.Neovim q = vim_command "qa!"
        testRunner <- async . void $ runReaderT (runResourceT q) actionCfg

        waitExitCode nvimProcess >>= \case
            ExitFailure i ->
                fail $ "Neovim returned with an exit status of: " ++ show i
            ExitSuccess -> case result of
                Nothing -> fail "Test timed out"
                Just _ -> pure ()
        cancel testRunner
        cleanUp

runInEmbeddedNeovim' :: TestConfiguration -> Neovim () a -> IO ()
runInEmbeddedNeovim' testCfg = runInEmbeddedNeovim testCfg ()

{-# DEPRECATED testWithEmbeddedNeovim "Use \"runInEmbeddedNeovim def env action\" and open files with nvim_command \"edit file\"" #-}

{- | The same as 'runInEmbeddedNeovim' with the given file opened via @nvim_command "edit file"@.
 - This method is kept for backwards compatibility.
-}
testWithEmbeddedNeovim ::
    -- | Optional path to a file that should be opened
    Maybe (Path b File) ->
    -- | Maximum time (in seconds) that a test is allowed to run
    Seconds ->
    -- | Read-only configuration
    env ->
    -- | Test case
    Neovim env a ->
    IO ()
testWithEmbeddedNeovim file timeoutAfter env action =
    runInEmbeddedNeovim def{cancelAfter = timeoutAfter} env (openTestFile <* action)
  where
    openTestFile = case file of
        Nothing -> pure ()
        Just f -> nvim_command $ pack $ "edit " ++ toFilePath f

catchIfNvimIsNotOnPath :: IOException -> IO ()
catchIfNvimIsNotOnPath e = case ioe_filename e of
    Just "nvim" ->
        putDoc . annotate (color Red) $
            vsep
                [ "The neovim executable 'nvim' is not on the PATH."
                , "You may not be testing fully!"
                ]
    _ ->
        throwIO e

startEmbeddedNvim ::
    Seconds ->
    IO (Process Handle Handle (), Internal.Config RPCConfig, IO ())
startEmbeddedNvim timeoutAfter = do
    nvimProcess <-
        startProcess $
            setStdin createPipe $
                setStdout createPipe $
                    proc "nvim" ["-n", "--clean", "--embed"]

    cfg <- Internal.newConfig (pure Nothing) newRPCConfig

    socketReader <-
        async . void $
            runSocketReader
                (getStdout nvimProcess)
                (cfg{Internal.pluginSettings = Nothing})

    eventHandler <-
        async . void $
            runEventHandler
                (getStdin nvimProcess)
                (cfg{Internal.pluginSettings = Nothing})

    atomically $
        putTMVar
            (Internal.globalFunctionMap cfg)
            (Internal.mkFunctionMap [])

    timeoutAsync <- async . void $ do
        threadDelay $ microSeconds timeoutAfter
        getExitCode nvimProcess >>= maybe (stopProcess nvimProcess) (\_ -> pure ())

    let cleanUp = mapM_ cancel [socketReader, eventHandler, timeoutAsync]

    pure (nvimProcess, cfg, cleanUp)
