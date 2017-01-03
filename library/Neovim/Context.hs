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
    Neovim',
    NeovimException(..),
    FunctionMap,
    FunctionMapEntry,
    mkFunctionMap,
    runNeovim,
    forkNeovim,
    err,
    errOnInvalidResult,
    restart,
    quit,

    ask,
    asks,
    get,
    gets,
    put,
    modify,

    throwError,
    module Control.Monad.IO.Class,
    ) where


import           Neovim.Classes
import           Neovim.Context.Internal      (FunctionMap, FunctionMapEntry,
                                               Neovim, Neovim', forkNeovim,
                                               mkFunctionMap,
                                               newUniqueFunctionName, runNeovim)
import           Neovim.Exceptions            (NeovimException (..))

import qualified Neovim.Context.Internal      as Internal


import           Control.Concurrent           (putMVar)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.MessagePack             (Object)
import           Text.PrettyPrint.ANSI.Leijen (Pretty (..))


-- | @'throw'@ specialized to a 'Pretty' value.
err :: Pretty err => err ->  Neovim r st a
err = throw . ErrorMessage . pretty


errOnInvalidResult :: (NvimObject o)
                   => Neovim r st (Either NeovimException Object)
                   -> Neovim r st o
errOnInvalidResult a = a >>= \case
    Left o ->
        (err . show) o

    Right o -> case fromObject o of
        Left e ->
            err e

        Right x ->
            return x


-- | Initiate a restart of the plugin provider.
restart :: Neovim r st ()
restart = liftIO . flip putMVar Internal.Restart =<< Internal.asks' Internal.transitionTo


-- | Initiate the termination of the plugin provider.
quit :: Neovim r st ()
quit = liftIO . flip putMVar Internal.Quit =<< Internal.asks' Internal.transitionTo

