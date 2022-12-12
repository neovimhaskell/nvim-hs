{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
    Seconds (..),
    TestConfiguration (..),
    -- deprecated
    testWithEmbeddedNeovim,
) where

import Neovim
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
import Neovim.Plugin (startPluginThreads)
import Neovim.Util (oneLineErrorMessage)
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
import UnliftIO (Handle, IOException, async, atomically, cancel, catch, newEmptyMVar, putMVar, putTMVar, throwIO, timeout)
import UnliftIO.Concurrent (takeMVar, threadDelay)

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
runInEmbeddedNeovim :: TestConfiguration -> Plugin env -> Neovim env a -> IO ()
runInEmbeddedNeovim TestConfiguration{..} plugin action =
    warnIfNvimIsNotOnPath runTest
  where
    runTest = do
        resultMVar <- newEmptyMVar
        let action' = do
                result <- action
                q <- Internal.asks' Internal.transitionTo
                putMVar q Internal.Quit
                -- vim_command isn't asynchronous, so we need to avoid waiting
                -- for the result of the operation by using 'async' since
                -- neovim cannot send a result if it has quit.
                _ <- async . void $ vim_command "qa!"
                putMVar resultMVar result
        (nvimProcess, cleanUp) <- startEmbeddedNvim cancelAfter plugin action'

        result <- timeout (microSeconds cancelAfter) (takeMVar resultMVar)

        waitExitCode nvimProcess >>= \case
            ExitFailure i ->
                fail $ "Neovim returned with an exit status of: " ++ show i
            ExitSuccess -> case result of
                Nothing -> fail "Test timed out"
                Just _ -> pure ()
        cleanUp

type TransitionHandler a = Internal.Config RPCConfig -> IO a

testTransitionHandler :: IO a -> TransitionHandler ()
testTransitionHandler onInitAction cfg =
    takeMVar (Internal.transitionTo cfg) >>= \case
        Internal.InitSuccess -> do
            void onInitAction
            testTransitionHandler onInitAction cfg
        Internal.Restart -> do
            fail "Restart unexpected"
        Internal.Failure e -> do
            fail . show $ oneLineErrorMessage e
        Internal.Quit -> do
            return ()

runInEmbeddedNeovim' :: TestConfiguration -> Neovim () a -> IO ()
runInEmbeddedNeovim' testCfg = runInEmbeddedNeovim testCfg Plugin{environment = (), exports = []}

{-# DEPRECATED testWithEmbeddedNeovim "Use \"runInEmbeddedNeovim def env action\" and open files with nvim_command \"edit file\"" #-}

{- | The same as 'runInEmbeddedNeovim' with the given file opened via @nvim_command "edit file"@.
 - This method is kept for backwards compatibility.
-}
testWithEmbeddedNeovim ::
    -- | Optional path to a file that should be opened
    Maybe FilePath ->
    -- | Maximum time (in seconds) that a test is allowed to run
    Seconds ->
    -- | Read-only configuration
    env ->
    -- | Test case
    Neovim env a ->
    IO ()
testWithEmbeddedNeovim file timeoutAfter env action =
    runInEmbeddedNeovim
        def{cancelAfter = timeoutAfter}
        Plugin{environment = env, exports = []}
        (openTestFile <* action)
  where
    openTestFile = case file of
        Nothing -> pure ()
        Just f -> nvim_command $ pack $ "edit " ++ f

warnIfNvimIsNotOnPath :: IO a -> IO ()
warnIfNvimIsNotOnPath test = void test `catch` \(e :: IOException) -> case ioe_filename e of
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
    Plugin env ->
    Neovim env () ->
    IO (Process Handle Handle (), IO ())
startEmbeddedNvim timeoutAfter plugin (Internal.Neovim action) = do
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

    let actionCfg = Internal.retypeConfig (environment plugin) cfg
        action' = runReaderT (runResourceT action) actionCfg
    pluginHandlers <-
        startPluginThreads (Internal.retypeConfig () cfg) [wrapPlugin plugin] >>= \case
            Left e -> do
                putMVar (Internal.transitionTo cfg) $ Internal.Failure e
                pure []
            Right (funMapEntries, pluginTids) -> do
                atomically $
                    putTMVar
                        (Internal.globalFunctionMap cfg)
                        (Internal.mkFunctionMap funMapEntries)
                putMVar (Internal.transitionTo cfg) Internal.InitSuccess
                pure pluginTids

    transitionHandler <- async . void $ do
        testTransitionHandler action' cfg
    timeoutAsync <- async . void $ do
        threadDelay $ microSeconds timeoutAfter
        getExitCode nvimProcess >>= maybe (stopProcess nvimProcess) (\_ -> pure ())

    let cleanUp =
            mapM_ cancel $
                [socketReader, eventHandler, timeoutAsync, transitionHandler]
                    ++ pluginHandlers

    pure (nvimProcess, cleanUp)
