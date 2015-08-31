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


import           Neovim.Context.Internal      (FunctionMap, FunctionMapEntry,
                                               Neovim, Neovim',
                                               NeovimException (ErrorMessage),
                                               forkNeovim, mkFunctionMap,
                                               newUniqueFunctionName, runNeovim)
import qualified Neovim.Context.Internal      as Internal

import           Control.Concurrent           (putMVar)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Text.PrettyPrint.ANSI.Leijen (Doc)


-- | @'throw'@ specialized to 'NeovimException'. This allows you to use the
-- 'IsString' instance to conventiently print error messages in neovim.
err :: Doc ->  Neovim r st a
err = throw . ErrorMessage


-- | Initiate a restart of the plugin provider.
restart :: Neovim r st ()
restart = liftIO . flip putMVar Internal.Restart =<< Internal.asks' Internal.transitionTo


-- | Initiate the termination of the plugin provider.
quit :: Neovim r st ()
quit = liftIO . flip putMVar Internal.Quit =<< Internal.asks' Internal.transitionTo

