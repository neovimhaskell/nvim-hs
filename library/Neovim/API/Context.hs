{- |
Module      :  Neovim.API.Context
Description :  The neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Context (
    Message(..),
    Neovim,
    InternalEnvironment(..),
    newInternalEnvironment,
    runNeovim,
    acall,
    acall',
    scall,
    scall',
    atomically',
    wait,
    wait',

    module Control.Monad.IO.Class,
    ) where

import Neovim.API.Classes

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.MessagePack
import Data.Monoid
import Data.Time

data InternalEnvironment a = InternalEnvironment
    { eventQueue :: TQueue Message
    , recipients :: TVar (Map Int64 (UTCTime, TMVar (Either Object Object)))
    , custom     :: a
    }

newInternalEnvironment :: (Applicative io, MonadIO io)
                       => a -> io (InternalEnvironment a)
newInternalEnvironment a = InternalEnvironment
    <$> liftIO newTQueueIO
    <*> liftIO (newTVarIO mempty)
    <*> pure a

data Message = FunctionCall String Object (TMVar (Either Object Object)) UTCTime

type Neovim cfg state = StateT state (ReaderT (InternalEnvironment cfg) IO)
    --deriving ( Functor, Applicative, Monad , MonadIO
             --, MonadReader (InternalEnvironment cfg), MonadState state)

--instance MonadBase IO (Neovim r st) where
    --liftBase = liftIO

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: InternalEnvironment r -> st -> Neovim r st a -> IO (a, st)
runNeovim r st a = runReaderT (runStateT a st) r


unexpectedException :: String -> err -> a
unexpectedException fn _ = error $
    "Function threw an exception even though it was declared not to throw one: "
    <> fn

withIgnoredException :: (Functor f, NvimObject result)
                     => String -- ^ Function name for better error messages
                     -> f (Either err result)
                     -> f result
withIgnoredException fn = fmap (either (unexpectedException fn) id)

-- | Helper function that concurrently puts a 'Message' in the event queue
-- and returns an 'STM' action that returns the result.
acall :: (NvimObject result)
     => String
     -> [Object]
     -> Neovim r st (STM (Either Object result))
acall fn parameters = do
    q <- asks eventQueue
    mv <- liftIO newEmptyTMVarIO
    timestamp <- liftIO getCurrentTime
    atomically' . writeTQueue q $ FunctionCall fn (ObjectArray parameters) mv timestamp
    return $ either Left (Right . fromObjectUnsafe) <$> readTMVar mv

acall' :: (NvimObject result)
       => String
       -> [Object]
       -> Neovim r st (STM result)
acall' fn parameters = withIgnoredException fn <$> acall fn parameters

-- | Call a neovim function synchronously. This function blocks until the
-- result is available.
scall :: (NvimObject result)
      => String        -- ^ Function name
      -> [Object]      -- ^ Parameters in an 'Object' array
      -> Neovim r st (Either Object result)
      -- ^ result value of the call or the thrown exception
scall fn parameters = acall fn parameters >>= atomically'

scall' :: NvimObject result => String -> [Object] -> Neovim r st result
scall' fn = withIgnoredException fn . scall fn

-- | Lifted variant of 'atomically'.
atomically' :: (MonadIO io) => STM result -> io result
atomically' = liftIO . atomically

-- | Wait for the result of the STM action.
--
-- This action possibly blocks as it is an alias for
-- @ \ioSTM -> ioSTM >>= liftIO . atomically@.
wait :: (MonadIO io) => io (STM result) -> io result
wait = (=<<) atomically'

-- | Variant of 'wait' that discards the result.
wait' :: (Functor io, MonadIO io) => io (STM result) -> io ()
wait' = void . (=<<) atomically'
