{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
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
    waitErr,
    waitErr',
    respond,
    ) where

import           Neovim.Classes
import           Neovim.Context
import           Neovim.Plugin.IPC
import           Neovim.Plugin.IPC.Internal

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader       as R
import           Data.MessagePack
import           Data.Monoid
import           Data.Text

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
    q <- eventQueue
    mv <- liftIO newEmptyTMVarIO
    timestamp <- liftIO getCurrentTime
    atomically' . writeTQueue q . SomeMessage $ FunctionCall fn parameters mv timestamp
    return $ convertObject <$> readTMVar mv
  where
    convertObject = \case
        Left e -> Left e
        Right o -> case fromObject o of
            Left e -> Left (toObject e)
            Right r -> Right r


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
wait :: Neovim r st (STM result) -> Neovim r st result
wait = (=<<) atomically'


-- | Variant of 'wait' that discards the result.
wait' :: Neovim r st (STM result) -> Neovim r st ()
wait' = void . wait


-- | Wait for the result of the 'STM' action and call @'err' . (loc++) . show@
-- if the action returned an error.
waitErr :: (Show e)
        => String                              -- ^ Prefix error message with this.
        -> Neovim r st (STM (Either e result)) -- ^ Function call to neovim
        -> Neovim r st result
waitErr loc act = wait act >>= either (err . (loc++) . show) return


-- | 'waitErr' that discards the result.
waitErr' :: (Show e)
         => String
         -> Neovim r st (STM (Either e result))
         -> Neovim r st ()
waitErr' loc = void . waitErr loc


-- | Send the result back to the neovim instance.
respond :: (NvimObject result) => Request -> Either String result -> Neovim r st ()
respond Request{..} result = do
    q <- eventQueue
    atomically' . writeTQueue q . SomeMessage $ uncurry (Response reqId) oResult

  where
    oResult = case result of
        Left e   -> (toObject e, toObject ())
        Right r  -> (toObject (), toObject r)

