{- |
Module      :  Neovim.Main
Description :  Wrapper for the actual main function
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.Main
    where

import Neovim.Config

import qualified Config.Dyre as Dyre
import Options.Applicative
import System.IO (stdin, stdout)
import Neovim.RPC.SocketReader
import Neovim.RPC.EventHandler
import Neovim.RPC.Common
import Neovim.API.Context
import Neovim.Debug
import Control.Concurrent
import Data.Maybe
import Control.Monad

data CommandLineOptions =
    Opt { hostPort   :: Maybe (String, Int)
        , unix       :: Maybe (FilePath)
        , env        :: Bool
        }

optParser :: Parser CommandLineOptions
optParser = Opt
    <$> optional ((,)
            <$> (strOption
                (long "host"
                <> short 'h'
                <> metavar "HOSTNAME"
                <> help "Connect to the specified host. (requires -p)"))
            <*> (option auto
                (long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Connect to the specified port. (requires -h)")))
    <*> optional (strOption
        (long "unix"
        <> short 'u'
        <> help "Connect to the given unix domain socket."))
    <*> switch
        ( long "environment"
        <> short 'e'
        <> help "Read connection information from $NVIM_LISTEN_ADDRESS.")

opts :: ParserInfo CommandLineOptions
opts = info (helper <*> optParser)
    (fullDesc
    <> header "Start a neovim plugin provider for Haskell plugins."
    <> progDesc "This is still work in progress. Feel free to contribute.")


neovim :: NeovimConfig -> IO ()
neovim = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.showError   = \cfg err -> cfg { errorMessage = Just err }
    , Dyre.projectName = "nvim"
    , Dyre.realMain    = realMain
    , Dyre.statusOut   = debugM "Dyre"
    }

realMain :: NeovimConfig -> IO ()
realMain cfg = maybe disableLogger (uncurry withLogger) (logOptions cfg) $ do
    os <- execParser opts
    runPluginProvider os cfg

runPluginProvider :: CommandLineOptions -> NeovimConfig -> IO ()
runPluginProvider os = case (hostPort os, unix os) of
    (Just (h,p), _) -> let s = TCP p h in run s s
    (_, Just fp)    -> let s = UnixSocket fp in run s s
    _ | env os      -> run Environment Environment
    _               -> run (Stdout stdout) (Stdout stdin)


  where
    run evHandlerSocket sockreaderSocket cfg = do
        e <- newInternalEnvironment
        ehTid <- forkIO $ runEventHandler evHandlerSocket e
        pluginTids <- forM (plugins cfg) $ forkIO . runNeovim e
        runSocketReader sockreaderSocket e
        forM_ (ehTid:pluginTids) $ \tid ->
            -- TODO saep 2014-12-02 Throw exceptions
            -- e.g. to allow registerig of cleanup functions
            return ()

