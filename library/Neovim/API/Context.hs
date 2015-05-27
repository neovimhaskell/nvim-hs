{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{- |
Module      :  Neovim.API.Context
Description :  The Neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Context (
    asks,
    ask,
    eventQueue,

    get,
    put,
    modify,
    gets,

    Neovim,
    Neovim',
    ConfigWrapper(..),
    runNeovim,
    err,

    throwError,
    module Control.Monad.IO.Class,
    ) where


import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader   hiding (ask, asks)
import qualified Control.Monad.Reader   as R
import           Control.Monad.State
import           Data.Data              (Typeable)
import           Neovim.API.IPC         (SomeMessage)

-- | A wrapper for a reader value that contains extra fields required to
-- communicate with the messagepack-rpc components.
data ConfigWrapper a = ConfigWrapper
    { _eventQueue  :: TQueue SomeMessage
    -- ^ A queue of messages that the event handler will propagate to
    -- appropriate threads and handlers.
    , customConfig :: a
    -- ^ Plugin author supplyable custom configuration. It can be queried via
    -- 'myConf'.
    }

eventQueue :: Neovim r st (TQueue SomeMessage)
eventQueue = R.asks _eventQueue

type Neovim cfg state = StateT state (ReaderT (ConfigWrapper cfg) IO)

type Neovim' = Neovim () ()

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: ConfigWrapper r
          -> st
          -> Neovim r st a
          -> IO (Either String (a, st))
runNeovim r st a = (try . runReaderT (runStateT a st)) r >>= \case
    Left e -> let e' = e :: SomeException
                  -- We want to catch everything here as the plugin provider
                  -- should not crash.  The error message is propagated to
                  -- neovim this way. In the future, we may want to handle
                  -- some kinds of exceptions here manually.
              in return . Left $ show e'
    Right res -> return $ Right res

data NeovimException = ErrorMessage String
    deriving (Typeable, Show)

instance Exception NeovimException

-- | @throw . ErrorMessage@
err :: String ->  Neovim r st a
err = throw . ErrorMessage

-- | Retrieve something from the configuration with respect to the first
-- function. Works exactly like 'R.asks'.
asks :: (config -> a) -> Neovim config state a
asks q = R.asks (q . customConfig)

-- | Retrieve the Cunfiguration (i.e. read-only state) from the 'Neovim'
-- context.
ask :: Neovim config state config
ask = R.asks customConfig

