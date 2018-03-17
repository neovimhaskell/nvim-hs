{-# LANGUAGE CPP        #-}
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
#if __GLASGOW_HASKELL__ <= 710
    module Control.Applicative,
#endif
    ) where


import           Neovim.Classes
import           Neovim.Context.Internal      (FunctionMap, FunctionMapEntry,
                                               Neovim, Neovim', forkNeovim,
                                               mkFunctionMap,
                                               newUniqueFunctionName, runNeovim)
import           Neovim.Exceptions            (NeovimException (..))

import qualified Neovim.Context.Internal      as Internal

#if __GLASGOW_HASKELL__ <= 710
import           Control.Applicative
#endif

import           Control.Concurrent           (putMVar)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.MessagePack             (Object)
import           Text.PrettyPrint.ANSI.Leijen (Pretty (..))


-- | @'throw'@ specialized to a 'Pretty' value.
err :: Pretty err => err ->  Neovim env a
err = throw . ErrorMessage . pretty


errOnInvalidResult :: (NvimObject o)
                   => Neovim env (Either NeovimException Object)
                   -> Neovim env o
errOnInvalidResult a = a >>= \case
    Left o ->
        (err . show) o

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

