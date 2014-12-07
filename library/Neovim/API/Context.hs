{- |
Module      :  Neovim.API.Context
Description :  The Neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Context (
    myConf,

    Message(..),
    Neovim,
    InternalEnvironment(..),
    newInternalEnvironment,
    runNeovim,

    module Control.Monad.IO.Class,
    ) where

import           Neovim.API.IPC

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map               (Map)
import           Data.MessagePack
import           Data.Monoid
import           Data.Text
import           Data.Time
import           Data.Word

data InternalEnvironment a = InternalEnvironment
    { eventQueue   :: TQueue SomeMessage
    -- ^ A queue of messages that the event handler will propagate to
    -- appropriate threads and handlers.
    , recipients   :: TVar (Map Word32 (UTCTime, TMVar (Either Object Object)))
    -- ^ A map from message identifiers (as per RPC spec) to a tuple with a
    -- timestamp and a 'TMVar' that is used to communicate the result back to
    -- the calling thread.
    , providers    :: Map Text (Either (Object -> IO (Either Object Object)) (TQueue SomeMessage))
    -- ^ A map that contains the function names which are registered to this
    -- plugin manager.
    , customConfig :: a
    -- ^ Plugin author supplyable custom configuration. It can be queried via
    -- 'myConf'.
    }

newInternalEnvironment :: (Applicative io, MonadIO io)
                       => a -> io (InternalEnvironment a)
newInternalEnvironment a = InternalEnvironment
    <$> liftIO newTQueueIO
    <*> liftIO (newTVarIO mempty)
    <*> pure mempty
    <*> pure a

type Neovim cfg state = StateT state (ReaderT (InternalEnvironment cfg) IO)

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: InternalEnvironment r -> st -> Neovim r st a -> IO (a, st)
runNeovim r st a = runReaderT (runStateT a st) r

-- | Retrieve the Cunfiguration (i.e. read-only state) from the 'Neovim'
-- context.
myConf :: Neovim config state config
myConf = asks customConfig

