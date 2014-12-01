{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    Neovim(..),
    InternalEnvironment(..),
    newInternalEnvironment,
    runNeovim,
    acall,
    acall',
    scall,
    scall',
    atomically',

    module Control.Monad.IO.Class
    ) where

import Neovim.API.Classes

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.MessagePack
import Data.Monoid
import Data.Time

data InternalEnvironment = InternalEnvironment
    { eventQueue :: TQueue Message
    , recipients :: TVar (Map Int64 (UTCTime, TMVar (Either Object Object)))
    }

newInternalEnvironment :: (Applicative io, MonadIO io)
                       => io InternalEnvironment
newInternalEnvironment = InternalEnvironment
    <$> liftIO newTQueueIO
    <*> liftIO (newTVarIO mempty)

data Message = FunctionCall String Object (TMVar (Either Object Object)) UTCTime

newtype Neovim a = Neovim (ReaderT InternalEnvironment IO a)
    deriving ( Functor, Applicative, Monad, MonadReader InternalEnvironment
             , MonadIO)

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: InternalEnvironment -> Neovim a -> IO a
runNeovim env (Neovim a) = runReaderT a env

unexpectedException :: String -> err -> a
unexpectedException fn _ = error $
    "Function threw an exception even though it was declared not to throw one: "
    <> fn

withIgnoredException :: (Functor f, NvimInstance result)
                     => String -- ^ Function name for better error messages
                     -> f (Either err result)
                     -> f result
withIgnoredException fn = fmap (either (unexpectedException fn) id)

-- | Helper function that concurrently puts a 'Message' in the event queue
-- and returns an 'STM' action that returns the result.
acall :: (NvimInstance result)
     => String
     -> [Object]
     -> Neovim (STM (Either Object result))
acall fn parameters = do
    q <- asks eventQueue
    mv <- liftIO newEmptyTMVarIO
    timestamp <- liftIO getCurrentTime
    atomically' . writeTQueue q $ FunctionCall fn (ObjectArray parameters) mv timestamp
    return $ either (Left . fromObject) (Right . fromObject) <$> readTMVar mv

acall' :: (NvimInstance result)
       => String
       -> [Object]
       -> Neovim (STM result)
acall' fn parameters = withIgnoredException fn <$> acall fn parameters

-- | Call a neovim function synchronously. This function blocks until the
-- result is available.
scall :: (NvimInstance result)
      => String        -- ^ Function name
      -> [Object]      -- ^ Parameters in an 'Object' array
      -> Neovim (Either Object result)
      -- ^ result value of the call or the thrown exception
scall fn parameters = acall fn parameters >>= atomically'

scall' :: NvimInstance result => String -> [Object] -> Neovim result
scall' fn = withIgnoredException fn . scall fn

-- | Lifted variant of 'atomically'.
atomically' :: (MonadIO io) => STM result -> io result
atomically' = liftIO . atomically

