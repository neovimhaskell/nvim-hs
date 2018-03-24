{-# LANGUAGE LambdaCase        #-}
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
    testWithEmbeddedNeovim,
    Seconds(..),
    ) where

import           Neovim
import qualified Neovim.Context.Internal                   as Internal
import           Neovim.RPC.Common                         (RPCConfig,
                                                            newRPCConfig)
import           Neovim.RPC.EventHandler                   (runEventHandler)
import           Neovim.RPC.SocketReader                   (runSocketReader)

import           Control.Monad.Reader                      (runReaderT)
import           Control.Monad.Trans.Resource              (runResourceT)
import           Data.Text.Prettyprint.Doc                 (annotate, vsep,
                                                            (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (Color (..), color,
                                                            putDoc)
import           GHC.IO.Exception                          (ioe_filename)
import           System.Directory
import           System.Exit                               (ExitCode (..))
import           System.IO                                 (Handle)
import           System.Process
import           UnliftIO.Async                            (async, cancel)
import           UnliftIO.Concurrent                       (threadDelay)
import           UnliftIO.Exception
import           UnliftIO.STM                              (atomically,
                                                            putTMVar)


-- | Type synonym for 'Word'.
newtype Seconds = Seconds Word


-- | Run the given 'Neovim' action according to the given parameters.
-- The embedded neovim instance is started without a config (i.e. it is passed
-- @-u NONE@).
--
-- If you want to run your tests purely from haskell, you have to setup
-- the desired state of neovim with the help of the functions in
-- "Neovim.API.String".
testWithEmbeddedNeovim
    :: Maybe FilePath -- ^ Optional path to a file that should be opened
    -> Seconds        -- ^ Maximum time (in seconds) that a test is allowed to run
    -> env            -- ^ Read-only configuration
    -> Neovim env a   -- ^ Test case
    -> IO ()
testWithEmbeddedNeovim file timeout r (Internal.Neovim a) =
    runTest `catch` catchIfNvimIsNotOnPath
  where
    runTest = do
        (_, _, ph, cfg, cleanUp) <- startEmbeddedNvim file timeout

        let testCfg = Internal.retypeConfig r cfg

        void $ runReaderT (runResourceT a) testCfg

        -- vim_command isn't asynchronous, so we need to avoid waiting for the
        -- result of the operation since neovim cannot send a result if it
        -- has quit.
        let Internal.Neovim q = vim_command "qa!"
        testRunner <- async . void $ runReaderT (runResourceT q) testCfg

        waitForProcess ph >>= \case
            ExitFailure i ->
                fail $ "Neovim returned with an exit status of: " ++ show i

            ExitSuccess ->
                return ()
        cancel testRunner
        cleanUp


catchIfNvimIsNotOnPath :: IOException -> IO ()
catchIfNvimIsNotOnPath e = case ioe_filename e of
    Just "nvim" ->
        putDoc . annotate (color Red) $ vsep
            [ "The neovim executable 'nvim' is not on the PATH."
            , "You may not be testing fully!"
            ]

    _           ->
        throwIO e

startEmbeddedNvim
    :: Maybe FilePath
    -> Seconds
    -> IO (Handle, Handle, ProcessHandle, Internal.Config RPCConfig, IO ())
startEmbeddedNvim file (Seconds timeout) = do
    args <- case file of
                Nothing ->
                    return []

                Just f -> do
                    -- 'fail' should work with most testing frameworks. In case
                    -- it doesn't, please file a bug report!
                    unlessM (doesFileExist f) . fail $ "File not found: " ++ f
                    return [f]

    (Just hin, Just hout, _, ph) <-
        createProcess (proc "nvim" (["-n","-u","NONE","--embed"] ++ args))
            { std_in = CreatePipe
            , std_out = CreatePipe
            }

    cfg <- Internal.newConfig (pure Nothing) newRPCConfig

    socketReader <- async . void $ runSocketReader
                    hout
                    (cfg { Internal.pluginSettings = Nothing })

    eventHandler <- async . void $ runEventHandler
                    hin
                    (cfg { Internal.pluginSettings = Nothing })

    atomically $ putTMVar
                    (Internal.globalFunctionMap cfg)
                    (Internal.mkFunctionMap [])

    timeoutAsync <- async . void $ do
        threadDelay $ (fromIntegral timeout) * 1000 * 1000
        getProcessExitCode ph >>= maybe (terminateProcess ph) (\_ -> return ())

    let cleanUp = mapM_ cancel [socketReader, eventHandler, timeoutAsync]

    return (hin, hout, ph, cfg, cleanUp)

