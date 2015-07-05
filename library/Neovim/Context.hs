{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{- |
Module      :  Neovim.Context
Description :  The Neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.Context (
    asks,
    ask,
    eventQueue,

    get,
    put,
    modify,
    gets,

    Neovim,
    Neovim',
    NeovimException(..),
    ConfigWrapper(..),
    runNeovim,
    forkNeovim,
    err,
    restart,
    quit,
    QuitAction(..),

    throwError,
    module Control.Monad.IO.Class,
    ) where


import           Control.Concurrent     (ThreadId, forkIO, putMVar, MVar)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader   hiding (ask, asks)
import qualified Control.Monad.Reader   as R
import           Control.Monad.State
import           Data.Data              (Typeable)
import           Neovim.Plugin.IPC         (SomeMessage)
import           System.Log.Logger

-- | A wrapper for a reader value that contains extra fields required to
-- communicate with the messagepack-rpc components.
data ConfigWrapper a = ConfigWrapper
    { _eventQueue   :: TQueue SomeMessage
    -- ^ A queue of messages that the event handler will propagate to
    -- appropriate threads and handlers.
    , _quit         :: MVar QuitAction
    -- ^ The main thread will wait for this 'MVar' to be filled with a value
    -- and then perform an action appropriate for the value of type
    -- 'QuitAction'.
    , _providerName :: String
    -- ^ Name that is used to identify this provider. Assigning such a name is
    -- done in the neovim config (e.g. ~\/.nvim\/nvimrc).
    , customConfig  :: a
    -- ^ Plugin author supplyable custom configuration. It can be queried via
    -- 'myConf'.
    }

data QuitAction = Quit
                -- ^ Quit the plugin provider.
                | Restart
                -- ^ Restart the plugin provider.
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

eventQueue :: Neovim r st (TQueue SomeMessage)
eventQueue = R.asks _eventQueue

type Neovim cfg state = StateT state (ReaderT (ConfigWrapper cfg) IO)

type Neovim' = Neovim () ()

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: ConfigWrapper r
          -> st
          -> Neovim r st a
          -> IO (Either String (a, st))
runNeovim r st a = (try . runReaderT (runStateT a st)) r >>= \case
    Left e -> do
        liftIO . errorM "Context" $ "Converting Exception to Error message: " ++ show e
        return . Left $ show (e :: SomeException)
    Right res -> return $ Right res

-- | Fork a neovim thread with the given custom config value and a custom
-- state. The result of the thread is discarded and only the 'ThreadId' is
-- returend immediately.
forkNeovim :: ir -> ist -> Neovim ir ist a -> Neovim r st ThreadId
forkNeovim r st a = do
    cfg <- R.ask
    liftIO . forkIO . void $ runNeovim (cfg { customConfig = r }) st a

data NeovimException
    = ErrorMessage String
    deriving (Typeable, Show)

instance Exception NeovimException

-- | @throw . ErrorMessage@
err :: String ->  Neovim r st a
err = throw . ErrorMessage

-- | Retrieve something from the configuration with respect to the first
-- function. Works exactly like 'R.asks'.
asks :: (config -> a) -> Neovim config state a
asks q = R.asks (q . customConfig)

-- | Retrieve the Cunfiguration (i.e. read-only state) from the 'Neovim'
-- context.
ask :: Neovim config state config
ask = R.asks customConfig

-- | Initiate a restart of the plugin provider.
restart :: Neovim r st ()
restart = liftIO . flip putMVar Restart =<< R.asks _quit

-- | Initiate the termination of the plugin provider.
quit :: Neovim r st ()
quit = liftIO . flip putMVar Quit =<< R.asks _quit

