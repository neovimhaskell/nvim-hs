{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
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


import           Neovim.Context.Internal (FunctionMap, FunctionMapEntry, Neovim,
                                          Neovim', forkNeovim, mkFunctionMap,
                                          newUniqueFunctionName, runNeovim)
import qualified Neovim.Context.Internal as Internal

import           Control.Concurrent      (putMVar)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Data               (Typeable)


-- | Exceptions specific to /nvim-hs/.
data NeovimException
    = ErrorMessage String
    -- ^ Simply error message that is passed to neovim. It should currently only
    -- contain one line of text.
    deriving (Typeable, Show)

instance Exception NeovimException


-- | @throw . ErrorMessage@
err :: String ->  Neovim r st a
err = throw . ErrorMessage


-- | Initiate a restart of the plugin provider.
restart :: Neovim r st ()
restart = liftIO . flip putMVar Internal.Restart =<< Internal.asks' Internal.transitionTo


-- | Initiate the termination of the plugin provider.
quit :: Neovim r st ()
quit = liftIO . flip putMVar Internal.Quit =<< Internal.asks' Internal.transitionTo

