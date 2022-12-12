{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.Context
Description :  The Neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
-}
module Neovim.Context (
    newUniqueFunctionName,
    Neovim,
    NeovimException (..),
    exceptionToDoc,
    FunctionMap,
    FunctionMapEntry,
    mkFunctionMap,
    runNeovim,
    err,
    errOnInvalidResult,
    restart,
    quit,
    subscribe,
    unsubscribe,
    ask,
    asks,
    get,
    gets,
    put,
    modify,
    Doc,
    AnsiStyle,
    docToText,
    throwError,
    module Control.Monad.IO.Class,
) where

import Neovim.Classes
import Neovim.Context.Internal (
    FunctionMap,
    FunctionMapEntry,
    Neovim,
    mkFunctionMap,
    newUniqueFunctionName,
    runNeovim,
    subscribe,
    unsubscribe,
 )
import Neovim.Exceptions (NeovimException (..), exceptionToDoc)

import qualified Neovim.Context.Internal as Internal

import Control.Concurrent (putMVar)
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.MessagePack (Object)

-- | @'throw'@ specialized to a 'Pretty' value.
err :: Doc AnsiStyle -> Neovim env a
err = throw . ErrorMessage

errOnInvalidResult ::
    (NvimObject o) =>
    Neovim env (Either NeovimException Object) ->
    Neovim env o
errOnInvalidResult a =
    a >>= \case
        Left o ->
            (err . exceptionToDoc) o
        Right o -> case fromObject o of
            Left e ->
                err e
            Right x ->
                return x

-- | Initiate a restart of the plugin provider.
restart :: Neovim env ()
restart = liftIO . flip putMVar Internal.Restart =<< Internal.asks' Internal.transitionTo

-- | Initiate the termination of the plugin provider.
quit :: Neovim env ()
quit = liftIO . flip putMVar Internal.Quit =<< Internal.asks' Internal.transitionTo
