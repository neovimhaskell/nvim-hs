{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    acall,
    acallVoid,
    scall,
    atomically'
    ) where

import Neovim.API.Classes

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.MessagePack

data InternalEnvironment = InternalEnvironment
                         { eventQueue :: TQueue Message }

data Message = AsyncCall String Object (TMVar Object)
             | SyncCall  String Object (TMVar Object)
             | AsyncVoidFunctionCall String Object

newtype Neovim a = Neovim { runNeovim :: ReaderT InternalEnvironment IO a }
    deriving (Monad, Functor, Applicative, MonadReader InternalEnvironment, MonadIO)

-- | Helper function that concurrently puts a 'Message' in the event queue
-- and returns an 'STM' action that returns the result.
call :: (NvimInstance result)
     => (String -> Object -> TMVar Object -> Message)
     -> String
     -> [Object]
     -> Neovim (STM result)
call msgType fn parameters = do
    q <- asks eventQueue
    mv <- liftIO newEmptyTMVarIO
    atomically' . writeTQueue q $ msgType fn (ObjectArray parameters) mv
    return $ fromObject <$> readTMVar mv

-- | Call a neovim function asynchronously. If you need the value, you can call
-- 'atomically'' on the returned 'STMVar'.
acall :: (NvimInstance result)
      => String              -- ^ Function name
      -> [Object]            -- ^ Parameters in an 'Object' array
      -> Neovim (STM result) -- ^ 'STM' action that will return the result
acall = call AsyncCall

-- | Call a neovim function synchronously. This function blocks until the
-- result is available.
scall :: (NvimInstance result)
      => String        -- ^ Function name
      -> [Object]      -- ^ Parameters in an 'Object' array
      -> Neovim result -- ^ result value of the call
scall fn parameters =
    call SyncCall fn parameters >>= atomically'

-- | Asynchronous remote function call without a return value.
acallVoid :: String -> [Object] -> Neovim ()
acallVoid fn parameters = do
    q <- asks eventQueue
    atomically' . writeTQueue q $ AsyncVoidFunctionCall fn (ObjectArray parameters)

-- | Lifted variant of 'atomically'.
atomically' :: (MonadIO io) => STM result -> io result
atomically' = liftIO . atomically

