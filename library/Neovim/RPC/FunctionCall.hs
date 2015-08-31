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
import qualified Neovim.Context.Internal   as Internal
import           Neovim.Plugin.Classes     (FunctionName)
import           Neovim.Plugin.IPC.Classes
import qualified Neovim.RPC.Classes        as MsgpackRPC

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.MessagePack
import           Data.Monoid
import qualified Text.PrettyPrint.ANSI.Leijen as P

import           Prelude

-- | Simply fail and call 'error' in case an unexpected exception is thrown.
-- This fails with a runtime exception. It is used by the Template Haskell API
-- generator for functions that are defined as not being able to fail. If this
-- exception occurs, it is a bug in neovim.
unexpectedException :: String -> err -> a
unexpectedException fn _ = error $
    "Function threw an exception even though it was declared not to throw one: "
    <> fn


-- | Strip the error result from the function call. This should only be used by
-- the Template Haskell API generated code for functions that declare
-- themselves as unfailable.
withIgnoredException :: (Functor f, NvimObject result)
                     => FunctionName -- ^ For better error messages
                     -> f (Either err result)
                     -> f result
withIgnoredException fn = fmap (either ((unexpectedException . show) fn) id)


-- | Helper function that concurrently puts a 'Message' in the event queue and returns an 'STM' action that returns the result.
acall :: (NvimObject result)
     => FunctionName
     -> [Object]
     -> Neovim r st (STM (Either Object result))
acall fn parameters = do
    q <- Internal.asks' Internal.eventQueue
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



-- | Helper function similar to 'acall' that throws a runtime exception if the
-- result is an error object.
acall' :: (NvimObject result)
       => FunctionName
       -> [Object]
       -> Neovim r st (STM result)
acall' fn parameters = withIgnoredException fn <$> acall fn parameters


-- | Call a neovim function synchronously. This function blocks until the
-- result is available.
scall :: (NvimObject result)
      => FunctionName
      -> [Object]      -- ^ Parameters in an 'Object' array
      -> Neovim r st (Either Object result)
      -- ^ result value of the call or the thrown exception
scall fn parameters = acall fn parameters >>= atomically'


-- | Helper function similar to 'scall' that throws a runtime exception if the
-- result is an error object.
scall' :: NvimObject result => FunctionName -> [Object] -> Neovim r st result
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
waitErr :: (P.Pretty e)
        => String                              -- ^ Prefix error message with this.
        -> Neovim r st (STM (Either e result)) -- ^ Function call to neovim
        -> Neovim r st result
waitErr loc act = wait act >>= either (err . (P.<>) (P.text loc) . P.pretty) return


-- | 'waitErr' that discards the result.
waitErr' :: (P.Pretty e)
         => String
         -> Neovim r st (STM (Either e result))
         -> Neovim r st ()
waitErr' loc = void . waitErr loc


-- | Send the result back to the neovim instance.
respond :: (NvimObject result) => Request -> Either String result -> Neovim r st ()
respond Request{..} result = do
    q <- Internal.asks' Internal.eventQueue
    atomically' . writeTQueue q . SomeMessage . MsgpackRPC.Response reqId $
        either (Left . toObject) (Right . toObject) result

