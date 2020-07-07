{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{- |
Module      :  Neovim.Context.Internal
Description :  Abstract description of the plugin provider's internal context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

To shorten function and data type names, import this qualfied as @Internal@.
-}
module Neovim.Context.Internal
    where

import           Neovim.Classes
import           Neovim.Exceptions                         (NeovimException (..),
                                                            exceptionToDoc)
import           Neovim.Plugin.Classes
import           Neovim.Plugin.IPC                         (SomeMessage)

import           Control.Applicative
import           Control.Exception                         (ArithException,
                                                            ArrayException,
                                                            ErrorCall,
                                                            PatternMatchFail)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.UTF8                      as U (fromString)
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.MessagePack                          (Object)
import           System.Log.Logger
import           UnliftIO

import           Data.Text.Prettyprint.Doc                 (viaShow)

import qualified Control.Monad.Fail as Fail
import           Prelude


-- | This is the environment in which all plugins are initially started.
--
-- Functions have to run in this transformer stack to communicate with neovim.
-- If parts of your own functions dont need to communicate with neovim, it is
-- good practice to factor them out. This allows you to write tests and spot
-- errors easier. Essentially, you should treat this similar to 'IO' in general
-- haskell programs.
newtype Neovim env a = Neovim
    { unNeovim :: ResourceT (ReaderT (Config env) IO) a }

  deriving (Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadUnliftIO)


-- | User facing instance declaration for the reader state.
instance MonadReader env (Neovim env) where
    ask = Neovim $ asks customConfig
    local f (Neovim a) = do
        r <- Neovim ask
        liftIO $ runReaderT (runResourceT a)
                    (r { customConfig = f (customConfig r)})


instance MonadResource (Neovim env) where
    liftResourceT m = Neovim $ liftResourceT m


instance Fail.MonadFail (Neovim env) where
  fail = throwIO . ErrorMessage . pretty


-- | Same as 'ask' for the 'InternalConfig'.
ask' :: Neovim env (Config env)
ask' = Neovim ask


-- | Same as 'asks' for the 'InternalConfig'.
asks' :: (Config env -> a) -> Neovim env a
asks' = Neovim . asks


exceptionHandlers :: [Handler IO (Either (Doc ann) a)]
exceptionHandlers =
    [ Handler $ \(_ :: ArithException)   -> ret "ArithException (e.g. division by 0)"
    , Handler $ \(_ :: ArrayException)   -> ret "ArrayException"
    , Handler $ \(_ :: ErrorCall)        -> ret "ErrorCall (e.g. call of undefined or error"
    , Handler $ \(_ :: PatternMatchFail) -> ret "Pattern match failure"
    , Handler $ \(_ :: SomeException)    -> ret "Unhandled exception"
    ]
  where
    ret = return . Left

-- | Initialize a 'Neovim' context by supplying an 'InternalEnvironment'.
runNeovim :: NFData a
          => Config env
          -> Neovim env a
          -> IO (Either (Doc AnsiStyle) a)
runNeovim = runNeovimInternal (\a -> a `deepseq` return a)

runNeovimInternal :: (a -> IO a)
                  -> Config env
                  -> Neovim env a
                  -> IO (Either (Doc AnsiStyle) a)
runNeovimInternal f r (Neovim a) =
    (try . runReaderT (runResourceT a)) r >>= \case
        Left e -> case fromException e of
            Just e' ->
                return . Left . exceptionToDoc $ (e' :: NeovimException)

            Nothing -> do
                liftIO . errorM "Context" $ "Converting Exception to Error message: " ++ show e
                (return . Left . viaShow) e
        Right res ->
            (Right <$> f res) `catches` exceptionHandlers


-- | Create a new unique function name. To prevent possible name clashes, digits
-- are stripped from the given suffix.
newUniqueFunctionName :: Neovim env FunctionName
newUniqueFunctionName = do
    tu <- asks' uniqueCounter
    -- reverseing the integer string should distribute the first character more
    -- evently and hence cause faster termination for comparisons.
    fmap (F . U.fromString . reverse . show) . liftIO . atomically $ do
        u <- readTVar tu
        modifyTVar' tu succ
        return u


-- | This data type is used to dispatch a remote function call to the appopriate
-- recipient.
newtype FunctionType = Stateful (TQueue SomeMessage)
    -- ^ 'Stateful' functions are handled within a special thread, the 'TQueue'
    -- is the communication endpoint for the arguments we have to pass.


instance Pretty FunctionType where
    pretty = \case
        Stateful  _ -> "\\os -> Neovim env o"


-- | Type of the values stored in the function map.
type FunctionMapEntry = (FunctionalityDescription, FunctionType)


-- | A function map is a map containing the names of functions as keys and some
-- context dependent value which contains all the necessary information to
-- execute that function in the intended way.
--
-- This type is only used internally and handles two distinct cases. One case
-- is a direct function call, wich is simply a function that accepts a list of
-- 'Object' values and returns a result in the 'Neovim' context. The second
-- case is calling a function that has a persistent state. This is mediated to
-- a thread that reads from a 'TQueue'. (NB: persistent currently means, that
-- state is stored for as long as the plugin provider is running and not
-- restarted.)
type FunctionMap = Map NvimMethod FunctionMapEntry


-- | Create a new function map from the given list of 'FunctionMapEntry' values.
mkFunctionMap :: [FunctionMapEntry] -> FunctionMap
mkFunctionMap = Map.fromList . map (\e -> (nvimMethod (fst e), e))


-- | A wrapper for a reader value that contains extra fields required to
-- communicate with the messagepack-rpc components and provide necessary data to
-- provide other globally available operations.
--
-- Note that you most probably do not want to change the fields prefixed with an
-- underscore.
data Config env = Config
    -- Global settings; initialized once
    { eventQueue        :: TQueue SomeMessage
    -- ^ A queue of messages that the event handler will propagate to
    -- appropriate threads and handlers.

    , transitionTo      :: MVar StateTransition
    -- ^ The main thread will wait for this 'MVar' to be filled with a value
    -- and then perform an action appropriate for the value of type
    -- 'StateTransition'.

    , providerName      :: TMVar (Either String Int)
    -- ^ Since nvim-hs must have its "Neovim.RPC.SocketReader" and
    -- "Neovim.RPC.EventHandler" running to determine the actual channel id
    -- (i.e. the 'Int' value here) this field can only be set properly later.
    -- Hence, the value of this field is put in an 'TMVar'.
    -- Name that is used to identify this provider. Assigning such a name is
    -- done in the neovim config (e.g. ~\/.nvim\/nvimrc).

    , uniqueCounter     :: TVar Integer
    -- ^ This 'TVar' is used to generate uniqe function names on the side of
    -- /nvim-hs/. This is useful if you don't want to overwrite existing
    -- functions or if you create autocmd functions.

    , globalFunctionMap :: TMVar FunctionMap
    -- ^ This map is used to dispatch received messagepack function calls to
    -- it's appropriate targets.

    -- Local settings; intialized for each stateful component
    , pluginSettings    :: Maybe (PluginSettings env)
    -- ^ In a registered functionality this field contains a function (and
    -- possibly some context dependent values) to register new functionality.

    , customConfig      :: env
    -- ^ Plugin author supplyable custom configuration. Queried on the
    -- user-facing side with 'ask' or 'asks'.
    }


-- | Convenient helper to create a new config for the given state and read-only
-- config.
--
-- Sets the 'pluginSettings' field to 'Nothing'.
retypeConfig :: env -> Config anotherEnv -> Config env
retypeConfig r cfg = cfg { pluginSettings = Nothing, customConfig = r }


-- | This GADT is used to share information between stateless and stateful
-- plugin threads since they work fundamentally in the same way. They both
-- contain a function to register some functionality in the plugin provider
-- as well as some values which are specific to the one or the other context.
data PluginSettings env where
    StatefulSettings
        :: (FunctionalityDescription
            -> ([Object] -> Neovim env Object)
            -> TQueue SomeMessage
            -> TVar (Map NvimMethod ([Object] -> Neovim env Object))
            -> Neovim env (Maybe FunctionMapEntry))
        -> TQueue SomeMessage
        -> TVar (Map NvimMethod ([Object] -> Neovim env Object))
        -> PluginSettings env


-- | Create a new 'InternalConfig' object by providing the minimal amount of
-- necessary information.
--
-- This function should only be called once per /nvim-hs/ session since the
-- arguments are shared across processes.
newConfig :: IO (Maybe String) -> IO env -> IO (Config env)
newConfig ioProviderName r = Config
    <$> newTQueueIO
    <*> newEmptyMVar
    <*> (maybe (atomically newEmptyTMVar) (newTMVarIO . Left) =<< ioProviderName)
    <*> newTVarIO 100
    <*> atomically newEmptyTMVar
    <*> pure Nothing
    <*> r


-- | The state that the plugin provider wants to transition to.
data StateTransition
    = Quit
    -- ^ Quit the plugin provider.

    | Restart
    -- ^ Restart the plugin provider.

    | Failure (Doc AnsiStyle)
    -- ^ The plugin provider failed to start or some other error occured.

    | InitSuccess
    -- ^ The plugin provider started successfully.

    deriving (Show)

