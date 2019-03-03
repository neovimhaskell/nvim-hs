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
import qualified Neovim.Context.Internal as Internal
import           Neovim.Log
import qualified Neovim.Plugin           as P
import           Neovim.Plugin.Startup   (StartupConfig (..))
import           Neovim.RPC.Common       as RPC
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader
import           Neovim.Util             (oneLineErrorMessage)

import qualified Config.Dyre            as Dyre
import qualified Config.Dyre.Relaunch   as Dyre
import           Control.Concurrent
import           Control.Concurrent.STM (atomically, putTMVar)
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative
import           System.IO              (stdin, stdout)
import           System.SetEnv
import           UnliftIO.Async         (Async, async, cancel)

import Prelude
import System.Environment


logger :: String
logger = "Neovim.Main"


data CommandLineOptions =
    Opt { providerName :: Maybe String
        , hostPort     :: Maybe (String, Int)
        , unix         :: Maybe FilePath
        , envVar       :: Bool
        , logOpts      :: Maybe (FilePath, Priority)
        }


instance Default CommandLineOptions where
    def = Opt
            { providerName = Nothing
            , hostPort     = Nothing
            , unix         = Nothing
            , envVar       = False
            , logOpts      = Nothing
            }


optParser :: Parser CommandLineOptions
optParser = Opt
    <$> optional (strArgument
        (metavar "NAME"
        <> help (unlines
                [ "Name that associates the plugin provider with neovim."
                , "This option has only an effect if you start nvim-hs"
                , "with rpcstart()/jobstart() and use the factory method approach."
                , "Since it is extremely hard to figure that out inside"
                , "nvim-hs, this option is assumed to used if the input"
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
neovim =
    let params = Dyre.defaultParams
            { Dyre.showError   = \cfg errM -> cfg { errorMessage = Just errM }
            , Dyre.projectName = "nvim"
            , Dyre.realMain    = realMain finishDyre (Just params)
            , Dyre.statusOut   = debugM "Dyre"
            , Dyre.ghcOpts     = ["-threaded", "-rtsopts", "-with-rtsopts=-N"]
            }
    in Dyre.wrapMain params


-- | Start the given plugins as a standalone plugin provider.
neovimStandalone
  :: [Internal.Neovim (StartupConfig NeovimConfig) P.NeovimPlugin]
  -> IO ()
neovimStandalone ps = realMain standalone Nothing Config
  { plugins = ps
  , logOptions = Nothing
  , errorMessage = Nothing
  }


-- | A 'TransitionHandler' function receives the 'ThreadId's of all running
-- threads which have been started by the plugin provider as well as the
-- 'Internal.Config' with the custom field set to 'RPCConfig'. These information
-- can be used to properly clean up a session and then do something else.
-- The transition handler is first called after the plugin provider has started.
type TransitionHandler a = [Async ()] -> Internal.Config RPCConfig -> IO a


-- | This main functions can be used to create a custom executable without
-- using the "Config.Dyre" library while still using the /nvim-hs/ specific
-- configuration facilities.
realMain :: TransitionHandler a
         -> Maybe (Dyre.Params NeovimConfig)
         -> NeovimConfig
         -> IO ()
realMain transitionHandler mParams cfg = do
    os <- execParser opts
    maybe disableLogger (uncurry withLogger) (logOpts os <|> logOptions cfg) $ do
        debugM logger "Starting up neovim haskell plguin provider"
        void $ runPluginProvider os (Just cfg) transitionHandler mParams


-- | Generic main function. Most arguments are optional or have sane defaults.
runPluginProvider
    :: CommandLineOptions -- ^ See /nvim-hs/ executables --help function or 'optParser'
    -> Maybe NeovimConfig
    -> TransitionHandler a
    -> Maybe (Dyre.Params NeovimConfig)
    -> IO a
runPluginProvider os mcfg transitionHandler mDyreParams = case (hostPort os, unix os) of
    (Just (h,p), _) ->
        createHandle (TCP p h) >>= \s -> run s s

    (_, Just fp) ->
        createHandle (UnixSocket fp) >>= \s -> run s s

    _ | envVar os ->
        createHandle Environment >>= \s -> run s s

    _ ->
        run stdout stdin

  where
    run evHandlerHandle sockreaderHandle = do

        -- The plugins to register depend on the given arguments and may need
        -- special initialization methods.
        let allPlugins = maybe [] plugins mcfg

        conf <- Internal.newConfig (pure (providerName os)) newRPCConfig

        ehTid <- async $ runEventHandler
                            evHandlerHandle
                            conf { Internal.pluginSettings = Nothing }

        srTid <- async $ runSocketReader sockreaderHandle conf

        ghcEnv <- forM ["GHC_PACKAGE_PATH","CABAL_SANDBOX_CONFIG"] $ \var -> do
            val <- lookupEnv var
            unsetEnv var
            return (var, val)
        let startupConf = Internal.retypeConfig
                            (StartupConfig mDyreParams ghcEnv)
                            conf
        P.startPluginThreads startupConf allPlugins >>= \case
            Left e -> do
                errorM logger $ "Error initializing plugins: " <> show (oneLineErrorMessage e)
                putMVar (Internal.transitionTo conf) $ Internal.Failure e
                transitionHandler [ehTid, srTid] conf

            Right (funMapEntries, pluginTids) -> do
                atomically $ putTMVar
                                (Internal.globalFunctionMap conf)
                                (Internal.mkFunctionMap funMapEntries)
                putMVar (Internal.transitionTo conf) $ Internal.InitSuccess
                transitionHandler (srTid:ehTid:pluginTids) conf


standalone :: TransitionHandler ()
standalone threads cfg = takeMVar (Internal.transitionTo cfg) >>= \case
    Internal.InitSuccess -> do
        debugM logger "Initialization Successful"
        standalone threads cfg

    Internal.Restart -> do
        errorM logger "Cannot restart"
        standalone threads cfg

    Internal.Failure e ->
        errorM logger . show $ oneLineErrorMessage e

    Internal.Quit ->
        return ()


-- | If the plugin provider is started with dyre, this handler is used to
-- handle a restart.
finishDyre :: TransitionHandler ()
finishDyre threads cfg = takeMVar (Internal.transitionTo cfg) >>= \case
    Internal.InitSuccess -> do
        debugM logger "Initialization Successful"
        finishDyre threads cfg

    Internal.Restart -> do
        debugM logger "Trying to restart nvim-hs"
        mapM_ cancel threads
        Dyre.relaunchMaster Nothing

    Internal.Failure e ->
        errorM logger . show $ oneLineErrorMessage e

    Internal.Quit ->
        return ()

