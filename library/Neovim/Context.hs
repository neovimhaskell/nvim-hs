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
    newUniqueFunctionName,

    get,
    put,
    modify,
    gets,

    Neovim,
    Neovim',
    NeovimException(..),
    ConfigWrapper(..),
    newConfigWrapper,
    runNeovim,
    forkNeovim,
    err,
    restart,
    quit,
    QuitAction(..),

    throwError,
    module Control.Monad.IO.Class,
    ) where


import           Control.Applicative
import           Control.Concurrent     (MVar, ThreadId, forkIO, putMVar, newEmptyMVar)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader   hiding (ask, asks)
import qualified Control.Monad.Reader   as R
import           Control.Monad.State
import           Data.Char              (isDigit)
import           Data.Data              (Typeable)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Neovim.Plugin.IPC      (SomeMessage)
import           System.Log.Logger


-- | A wrapper for a reader value that contains extra fields required to
-- communicate with the messagepack-rpc components and provide necessary data to
-- provide other globally available operations.
--
-- Note that you most probably do not want to change the fields prefixed with an
-- underscore.
--
-- TODO create lenses for access and hide constructor
data ConfigWrapper a = ConfigWrapper
    -- Global settings; initialized once
    { _eventQueue      :: TQueue SomeMessage
    -- ^ A queue of messages that the event handler will propagate to
    -- appropriate threads and handlers.

    , _quit            :: MVar QuitAction
    -- ^ The main thread will wait for this 'MVar' to be filled with a value
    -- and then perform an action appropriate for the value of type
    -- 'QuitAction'.

    , _providerName    :: String
    -- ^ Name that is used to identify this provider. Assigning such a name is
    -- done in the neovim config (e.g. ~\/.nvim\/nvimrc).

    , _uniqueCounter   :: TVar Integer
    -- ^ This 'TVar' is used to generate uniqe function names on the side of
    -- /nvim-hs/. This is useful if you don't want to overwrite existing
    -- functions or if you create autocmd functions.

    -- Local settings; intialized for each stateful component
    , _statefulChannel :: Maybe (TQueue SomeMessage)
    -- ^ This field is currently only used to properly register autocmd actions
    -- from within a plugin. It is the channel that the stateful plugin listens
    -- on or 'Nothing' if it is not a stateful plugin.

    , customConfig     :: a
    -- ^ Plugin author supplyable custom configuration. It can be queried via
    -- 'myConf'.
    }


-- | Create a new 'ConfigWrapper' object by providing the minimal amount of
-- necessary information.
--
-- This function should only be called once per /nvim-hs/ session since the
-- arguments are shared across processes.
newConfigWrapper :: IO String -> IO a -> IO (ConfigWrapper a)
newConfigWrapper providerName a = ConfigWrapper
    <$> newTQueueIO
    <*> newEmptyMVar
    <*> providerName
    <*> newTVarIO 100
    <*> pure Nothing
    <*> a


data QuitAction = Quit
                -- ^ Quit the plugin provider.
                | Restart
                -- ^ Restart the plugin provider.
                deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | Retrieve the reference for the local message queue. This can be ussed to
-- send arbitrary messages to other stateful threads.
--
-- TODO implement listening mechanism
eventQueue :: Neovim r st (TQueue SomeMessage)
eventQueue = R.asks _eventQueue


-- | Create a new unique function name. To prevent possible name clashes, digits
-- are stripped from the given suffix.
newUniqueFunctionName :: String -> Neovim r st Text
newUniqueFunctionName suffix = do
    let suffix' = filter isDigit suffix
    tu <- R.asks _uniqueCounter
    -- reverseing the integer string should distribute the first character more
    -- evently and hence cause faster termination for comparisons.
    fmap (Text.pack . (++suffix') . reverse . show) . liftIO . atomically $ do
        u <- readTVar tu
        modifyTVar' tu succ
        return u


-- | This is the environment in which all plugins are initially started.
-- Stateless functions use '()' for the static configuration and the mutable
-- state and there is another type alias for that case: 'Neovim''.
--
-- Functions have to run in this transformer stack to communicate with neovim.
-- If parts of your own functions dont need to communicate with neovim, it is
-- good practice to factor them out. This allows you to write tests and spot
-- errors easier. Essentially, you should treat this similar to 'IO' in general
-- haskell programs.
type Neovim r st = StateT st (ReaderT (ConfigWrapper r) IO)


-- | Convenience alias for @'Neovim' () ()@.
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
asks :: (r -> a) -> Neovim r st a
asks q = R.asks (q . customConfig)


-- | Retrieve the Cunfiguration (i.e. read-only state) from the 'Neovim'
-- context.
ask :: Neovim r st r
ask = R.asks customConfig


-- | Initiate a restart of the plugin provider.
restart :: Neovim r st ()
restart = liftIO . flip putMVar Restart =<< R.asks _quit


-- | Initiate the termination of the plugin provider.
quit :: Neovim r st ()
quit = liftIO . flip putMVar Quit =<< R.asks _quit

