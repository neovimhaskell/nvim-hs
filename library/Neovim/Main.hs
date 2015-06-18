{-# LANGUAGE LambdaCase #-}
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

import           Neovim.Plugin       as P
import           Neovim.Config
import qualified Neovim.Plugin.ConfigHelper as ConfigHelper

import qualified Config.Dyre             as Dyre
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Monoid
import           Neovim.API.Context
import           Neovim.Debug
import           Neovim.RPC.Common       as RPC
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader
import           Options.Applicative
import           System.IO               (stdin, stdout)

data CommandLineOptions =
    Opt { providerName :: String
        , hostPort     :: Maybe (String, Int)
        , unix         :: Maybe FilePath
        , env          :: Bool
        , logOpts      :: Maybe (FilePath, Priority)
        }

optParser :: Parser CommandLineOptions
optParser = Opt
    <$> strOption
        (long "name"
        <> short 'n'
        <> metavar "NAME"
        <> help "Name that associates the plugin provider with neovim")
    <*> optional ((,)
            <$> strOption
                (long "host"
                <> short 'a'
                <> metavar "HOSTNAME"
                <> help "Connect to the specified host. (requires -p)")
            <*> option auto
                (long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Connect to the specified port. (requires -a)"))
    <*> optional (strOption
        (long "unix"
        <> short 'u'
        <> help "Connect to the given unix domain socket."))
    <*> switch
        ( long "environment"
        <> short 'e'
        <> help "Read connection information from $NVIM_LISTEN_ADDRESS.")
    <*> optional ((,)
        <$> strOption
            (long "log-file"
            <> short 'l'
            <> help "File to log to.")
        <*> option auto
            (long "log-level"
            <> short 'v'
            <> help ("Log level. Must be one of: " ++ (unwords . map show) logLevels)))
  where
    -- [minBound..maxBound] would have been nice here.
    logLevels :: [Priority]
    logLevels = [ DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY ]


opts :: ParserInfo CommandLineOptions
opts = info (helper <*> optParser)
    (fullDesc
    <> header "Start a neovim plugin provider for Haskell plugins."
    <> progDesc "This is still work in progress. Feel free to contribute.")


neovim :: NeovimConfig -> IO ()
neovim conf =
    let params = Dyre.defaultParams
            { Dyre.showError   = \cfg errM -> cfg { errorMessage = Just errM }
            , Dyre.projectName = "nvim"
            , Dyre.realMain    = realMain
            , Dyre.statusOut   = debugM "Dyre"
            }
    in Dyre.wrapMain params (conf { dyreParams = Just params })

realMain :: NeovimConfig -> IO ()
realMain cfg = do
    os <- execParser opts
    maybe disableLogger (uncurry withLogger) (logOpts os <|> logOptions cfg) $ do
        logM "Neovim.Main" DEBUG "Starting up neovim haskell plguin provider"
        runPluginProvider os cfg

runPluginProvider :: CommandLineOptions -> NeovimConfig -> IO ()
runPluginProvider os = case (hostPort os, unix os) of
    (Just (h,p), _) -> let s = TCP p h in run s s
    (_, Just fp)    -> let s = UnixSocket fp in run s s
    _ | env os      -> run Environment Environment
    _               -> run (Stdout stdout) (Stdout stdin)

  where
    run evHandlerSocket sockreaderSocket cfg = do
        rpcConfig <- newRPCConfig
        q <- newTQueueIO
        let conf = ConfigWrapper q (providerName os) ()
            allPlugins = maybe id ((:) . ConfigHelper.plugin) (dyreParams cfg) $ plugins cfg
        startPluginThreads (conf { customConfig = RPC.functions rpcConfig }) allPlugins >>= \case
            Left e -> errorM "Neovim.Main" $ "Error initializing plugins: " <> e
            Right pluginTidsWithQueues -> do
                let rpcEnv = conf { customConfig = rpcConfig }
                ehTid <- forkIO $ runEventHandler evHandlerSocket rpcEnv
                _ <- forkIO $ register (conf { customConfig = RPC.functions rpcConfig }) pluginTidsWithQueues
                runSocketReader sockreaderSocket rpcEnv
                forM_ (ehTid:concatMap (map fst . snd) pluginTidsWithQueues) $ \tid ->
                    -- TODO actuall kill those threads in a reasonable way
                    liftIO $ debugM "Neovim.Main" $ "Killing thread" <> show tid

