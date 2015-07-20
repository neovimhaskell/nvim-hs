{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module      :  Neovim.Plugin
Description :
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin (
    startPluginThreads,
    register,
    wrapPlugin,
    NeovimPlugin,
    Plugin(..),
    Synchronous(..),
    CommandOptions(..),
    ) where

import           Neovim.API.String
import           Neovim.Classes
import           Neovim.Context
import           Neovim.Plugin.Classes
import           Neovim.Plugin.IPC
import           Neovim.Plugin.IPC.Internal
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Arrow              (first, (&&&))
import           Control.Concurrent         (ThreadId)
import           Control.Concurrent.STM
import           Control.Exception.Lifted   (SomeException, try)
import           Control.Monad              (foldM, forM, void)
import qualified Control.Monad.Reader       as R
import           Data.ByteString            (ByteString)
import           Data.Foldable              (forM_)
import qualified Data.Map                   as Map
import           Data.MessagePack
import           Data.Text                  (unpack)
import           System.Log.Logger

logger :: String
logger = "Neovim.Plugin"

startPluginThreads :: ConfigWrapper r
                   -> [IO NeovimPlugin]
                   -> IO (Either String [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])])
startPluginThreads cfg = fmap (fmap fst) . runNeovim cfg () . foldM go []
  where
    go :: [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
       -> IO NeovimPlugin
       -> Neovim r () [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
    go pluginThreads iop = do
        NeovimPlugin p <- liftIO iop
        tids <- mapM registerStatefulFunctionality (statefulExports p)
        return $ (NeovimPlugin p, tids) : pluginThreads

register :: ConfigWrapper (TMVar FunctionMap)
         -> [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
         -> IO ()
register (cfg@ConfigWrapper{ customConfig = sem }) ps = void . runNeovim cfg () $ do
    registeredFunctions <- forM ps $ \(NeovimPlugin p, fs) -> do
        let statefulFunctionsToRegister = concatMap snd fs
            statelessFunctionsToRegister =
                map (getDescription &&& Stateless . getFunction) $ exports p
            functionsToRegister = statefulFunctionsToRegister ++ statelessFunctionsToRegister
        mapM_ (registerWithNeovim . fst) functionsToRegister
        return $ map (first name) functionsToRegister
    liftIO . atomically . putTMVar sem . Map.fromList $ concat registeredFunctions

registerWithNeovim :: FunctionalityDescription -> Neovim customConfig () ()
registerWithNeovim = \case
    Function functionName s -> do
        pName <- R.asks _providerName
        ret <- wait $ vim_call_function "remote#define#FunctionOnHost"
            [ toObject pName, toObject functionName, toObject s
            , toObject functionName, toObject (Map.empty :: Map.Map ByteString Object)
            ]
        case ret of
            Left e -> liftIO . errorM logger $
                "Failed to register function: " ++ unpack functionName ++ show e
            Right _ -> liftIO . debugM logger $
                "Registered function: " ++ unpack functionName
    Command functionName copts -> do
        pName <- R.asks _providerName
        ret <- wait $ vim_call_function "remote#define#CommandOnHost"
            [ toObject pName, toObject functionName, toObject (cmdSync copts)
            , toObject functionName, toObject copts
            ]
        case ret of
            Left e -> liftIO . errorM logger $
                "Failed to register command: " ++ unpack functionName ++ show e
            Right _ -> liftIO . debugM logger $
                "Registered command: " ++ unpack functionName
    Autocmd acmdType functionName opts -> do
        pName <- R.asks _providerName
        ret <- wait $ vim_call_function "remote#define#AutocmdOnHost"
            [ toObject pName, toObject functionName, toObject (acmdSync opts)
            , toObject acmdType , toObject opts
            ]
        case ret of
            Left e -> liftIO . errorM logger $
                "Failed to register autocmd: " ++ unpack functionName ++ show e
            Right _ -> liftIO . debugM logger $
                "Registered autocmd: " ++ unpack functionName

-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: (r, st, [ExportedFunctionality r st])
    -> Neovim customcConfig () (ThreadId, [(FunctionalityDescription, FunctionType)])
registerStatefulFunctionality (r, st, fs) = do
    q <- liftIO newTQueueIO
    tid <- forkNeovim r st (listeningThread q)
    return (tid, map (\n -> (getDescription n, Stateful q)) fs)
  where
    functionRoutes = foldr updateRoute Map.empty fs
    updateRoute = uncurry Map.insert . (name &&& getFunction)

    executeFunction
        :: ([Object] -> Neovim r st Object)
        -> [Object]
        -> Neovim r st (Either String Object)
    executeFunction f args = try (f args) >>= \case
            Left e -> let e' = e :: SomeException
                      in return . Left $ show e'
            Right res -> return $ Right res
    listeningThread q = do
        msg <- liftIO . atomically $ readTQueue q
        forM_ (fromMessage msg) $ \req@Request{..} ->
            forM_ (Map.lookup reqMethod functionRoutes) $ \f ->
                respond req =<< executeFunction f reqArgs
        forM_ (fromMessage msg) $ \Notification{..} ->
            forM_ (Map.lookup notMethod functionRoutes) $ \f ->
                void $ executeFunction f notArgs
        listeningThread q

