{- |
Module      :  Neovim.RPC.FunctionCall
Description :  Functions for calling functions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.RPC.FunctionCall (
    acall,
    acall',
    scall,
    scall',
    atomically',
    wait,
    wait',
    ) where

import Neovim.API.Context
import Neovim.API.Classes
import Neovim.API.IPC

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Data.Text
import Data.Time
import Data.MessagePack
import Control.Concurrent.STM


unexpectedException :: String -> err -> a
unexpectedException fn _ = error $
    "Function threw an exception even though it was declared not to throw one: "
    <> fn

withIgnoredException :: (Functor f, NvimObject result)
                     => Text -- ^ Function name for better error messages
                     -> f (Either err result)
                     -> f result
withIgnoredException fn = fmap (either ((unexpectedException . unpack) fn) id)

-- | Helper function that concurrently puts a 'Message' in the event queue
-- and returns an 'STM' action that returns the result.
acall :: (NvimObject result)
     => Text
     -> [Object]
     -> Neovim r st (STM (Either Object result))
acall fn parameters = do
    q <- asks eventQueue
    mv <- liftIO newEmptyTMVarIO
    timestamp <- liftIO getCurrentTime
    atomically' . writeTQueue q . SomeMessage $ FunctionCall fn (ObjectArray parameters) mv timestamp
    return $ either Left (Right . fromObjectUnsafe) <$> readTMVar mv

acall' :: (NvimObject result)
       => Text
       -> [Object]
       -> Neovim r st (STM result)
acall' fn parameters = withIgnoredException fn <$> acall fn parameters

-- | Call a neovim function synchronously. This function blocks until the
-- result is available.
scall :: (NvimObject result)
      => Text        -- ^ Function name
      -> [Object]      -- ^ Parameters in an 'Object' array
      -> Neovim r st (Either Object result)
      -- ^ result value of the call or the thrown exception
scall fn parameters = acall fn parameters >>= atomically'

scall' :: NvimObject result => Text -> [Object] -> Neovim r st result
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
