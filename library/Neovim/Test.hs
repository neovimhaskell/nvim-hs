{-# LANGUAGE LambdaCase #-}
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
    ) where

import           Neovim
import qualified Neovim.Context.Internal      as Internal
import           Neovim.RPC.Common            (newRPCConfig, RPCConfig)
import           Neovim.RPC.EventHandler      (runEventHandler)
import           Neovim.RPC.SocketReader      (runSocketReader)

import           Control.Concurrent
import           Control.Concurrent.STM       (atomically, putTMVar)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (runStateT)
import           Control.Monad.Trans.Resource (runResourceT)
import           System.Directory
import           System.Exit                  (ExitCode (..))
import           System.IO                    (Handle)
import           System.Process


-- | Type synonym for 'Word'.
type Seconds = Word


-- | Run the given 'Neovim' action according to the given parameters.
--
-- Since we cannot really automate tests as run in a plugin provider
-- conveniently in pure Haskell code, this is the best you can get. You should
-- be able to test the functionality of one of your plugin functions this way,
-- though. You should use the function from "Neovim.API.String" or an equivalent
-- module to set the neovim set suitable for your use case.
--
testWithEmbeddedNeovim
    :: Maybe FilePath -- ^ Optional path to a file that should be opened
    -> Seconds        -- ^ Maximum time (in seconds) that a test is allowed to run
    -> r              -- ^ Read-only configuration
    -> st             -- ^ State
    -> Neovim r st a  -- ^ Test case
    -> IO ()
testWithEmbeddedNeovim file timeout r st (Internal.Neovim a) = do
    (_, _, ph, cfg) <- startEmbeddedNvim file timeout

    let testCfg = Internal.retypeConfig r st cfg

    void $ runReaderT (runStateT (runResourceT a) st) testCfg

    -- vim_command isn't asynchronous, so we need to avoid waiting for the
    -- result of the operation since neovim cannot send a result if it
    -- has quit.
    let Internal.Neovim q = vim_command "qa!"
    void . forkIO . void $ runReaderT (runStateT (runResourceT q) st ) testCfg

    waitForProcess ph >>= \case
        ExitFailure i -> fail $ "Neovim returned with an exit status of: " ++ show i
        ExitSuccess   -> return ()


startEmbeddedNvim :: Maybe FilePath
                  -> Word
                  -> IO (Handle, Handle, ProcessHandle, Internal.Config RPCConfig t)
startEmbeddedNvim file timeout = do
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

    _ <- forkIO $ runSocketReader hout (cfg { Internal.pluginSettings = Nothing })
    _ <- forkIO $ runEventHandler hin  (cfg { Internal.pluginSettings = Nothing })

    atomically $ putTMVar
                    (Internal.globalFunctionMap cfg)
                    (Internal.mkFunctionMap [])

    _ <- forkIO $ do
           threadDelay $ (fromIntegral timeout) * 1000 * 1000
           getProcessExitCode ph >>= maybe (terminateProcess ph) (const $ return ())

    return (hin, hout, ph, cfg)

