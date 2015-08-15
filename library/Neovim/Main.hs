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

import           Neovim.Config
import qualified Neovim.Context.Internal    as Internal
import           Neovim.Log
import           Neovim.Plugin              as P
import qualified Neovim.Plugin.ConfigHelper as ConfigHelper
import           Neovim.RPC.Common          as RPC
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader

import qualified Config.Dyre                as Dyre
import qualified Config.Dyre.Relaunch       as Dyre
import           Control.Concurrent
import           Control.Concurrent.STM     (putTMVar, atomically)
import           Control.Monad
import           Data.Monoid
import           Options.Applicative
import           System.IO                  (stdin, stdout)
import           System.SetEnv

import           System.Environment
import           Prelude


logger :: String
logger = "Neovim.Main"


data CommandLineOptions =
    Opt { providerName :: Maybe String
        , hostPort     :: Maybe (String, Int)
        , unix         :: Maybe FilePath
        , env          :: Bool
        , logOpts      :: Maybe (FilePath, Priority)
        }


instance Default CommandLineOptions where
    def = Opt
            { providerName = Nothing
            , hostPort     = Nothing
            , unix         = Nothing
            , env          = False
            , logOpts      = Nothing
            }

optParser :: Parser CommandLineOptions
optParser = Opt
    <$> optional (strArgument
        (metavar "NAME"
        <> help (unlines
                [ "Name that associates the plugin provider with neovim."
                , "This option has only an effect if you start nvim-hs"
                , "with rpcstart() and use the factory method approach."
                , "Since it is extremely hard to figure that out inside"
                , "nvim-hs, this options is assumed to used if the input"
                , "and output is tied to standard in and standard out."
                ])))
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


-- | This is essentially the main function for /nvim-hs/, at least if you want
-- to use "Config.Dyre" for the configuration.
neovim :: NeovimConfig -> IO ()
neovim conf =
    let params = Dyre.defaultParams
            { Dyre.showError   = \cfg errM -> cfg { errorMessage = Just errM }
            , Dyre.projectName = "nvim"
            , Dyre.realMain    = realMain finishDyre
            , Dyre.statusOut   = debugM "Dyre"
            , Dyre.ghcOpts     = ["-threaded", "-rtsopts", "-with-rtsopts=-N"]
            }
    in Dyre.wrapMain params (conf { dyreParams = Just params })


type Finalizer a = [ThreadId] -> Internal.Config RPCConfig () -> IO a


-- | This main functions can be used to create a custom executable without
-- using the "Config.Dyre" library while still using the /nvim-hs/ specific
-- configuration facilities.
realMain :: Finalizer a
         -> NeovimConfig
         -> IO ()
realMain finalizer cfg = do
    os <- execParser opts
    maybe disableLogger (uncurry withLogger) (logOpts os <|> logOptions cfg) $ do
        debugM logger "Starting up neovim haskell plguin provider"
        void $ runPluginProvider os (Just cfg) finalizer


-- | Generic main function. Most arguments are optional or have sane defaults.
runPluginProvider
    :: CommandLineOptions -- ^ See /nvim-hs/ executables --help function or 'optParser'
    -> Maybe NeovimConfig
    -> Finalizer a
    -> IO a
runPluginProvider os mcfg finalizer = case (hostPort os, unix os) of
    (Just (h,p), _) ->
        createHandle (TCP p h) >>= \s -> run s s

    (_, Just fp) ->
        createHandle (UnixSocket fp) >>= \s -> run s s

    _ | env os ->
        createHandle Environment >>= \s -> run s s

    _ ->
        run stdout stdin

  where
    run evHandlerHandle sockreaderHandle = do

        -- The plugins to register depend on the given arguments and may need
        -- special initialization methods.
        allPlugins <- case (fmap plugins mcfg, join (fmap dyreParams mcfg)) of
            -- No plugins should be loaded
            (Nothing, _) ->
                return []

            -- Some statically give plugins without Dyre support for the
            -- configuration.
            (Just ps, Nothing) ->
                return ps

            -- Full blown /nvim-hs/ with Dyre-based recompilation and plugins
            (Just ps, Just params) -> do
                -- Save environment variable state for recompilation with Dyre
                -- and unset those specific environment variables to avoid
                -- confusing other tools.
                ghcEnv <- forM ["GHC_PACKAGE_PATH","CABAL_SANDBOX_CONFIG"] $ \var -> do
                    val <- lookupEnv var
                    unsetEnv var
                    return (var, val)

                return (ConfigHelper.plugin ghcEnv params : ps)

        conf <- Internal.newConfig (pure (providerName os)) newRPCConfig

        ehTid <- forkIO $ runEventHandler
                            evHandlerHandle
                            conf { Internal.pluginSettings = Nothing }

        srTid <- forkIO $ runSocketReader sockreaderHandle conf

        startPluginThreads (Internal.retypeConfig () () conf) allPlugins >>= \case
            Left e -> do
                errorM logger $ "Error initializing plugins: " <> e
                putMVar (Internal.quit conf) $ Internal.Failure e
                finalizer [ehTid, srTid] conf

            Right (funMapEntries, pluginTids) -> do
                atomically $ putTMVar
                                (Internal.globalFunctionMap conf)
                                (Internal.mkFunctionMap funMapEntries)
                putMVar (Internal.quit conf) $ Internal.InitSuccess
                finalizer (srTid:ehTid:pluginTids) conf


finishDyre :: Finalizer ()
finishDyre threads cfg = readMVar (Internal.quit cfg) >>= \case
    Internal.InitSuccess -> do
        debugM logger "Waiting for threads to finish."
        finishDyre threads cfg

    Internal.Restart -> do
        debugM logger "Trying to restart nvim-hs"
        mapM_ killThread threads
        Dyre.relaunchMaster Nothing

    Internal.Failure e ->
        errorM logger e

    Internal.Quit ->
        return ()

