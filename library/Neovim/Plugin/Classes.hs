{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{- |
Module      :  Neovim.Plugin.Classes
Description :  Classes and data types related to plugins
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Classes (
    ExportedFunctionality(..),
    getFunction,
    getDescription,
    FunctionalityDescription(..),
    FunctionName(..),
    NeovimPlugin(..),
    Plugin(..),
    wrapPlugin,
    Synchronous(..),
    CommandOptions(..),
    AutocmdOptions(..),
    ) where

import           Neovim.Classes
import           Neovim.Context

import           Data.Default
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.Text        (Text)

-- | This data type is used in the plugin registration to properly register the
-- functions.
newtype ExportedFunctionality r st
    = EF (FunctionalityDescription, [Object] -> Neovim r st Object)


-- | Extract the description of an 'ExportedFunctionality'.
getDescription :: ExportedFunctionality r st -> FunctionalityDescription
getDescription (EF (d,_)) = d


-- | Extract the function of an 'ExportedFunctionality'.
getFunction :: ExportedFunctionality r st -> [Object] -> Neovim r st Object
getFunction (EF (_, f)) = f


-- | Functionality specific functional description entries.
data FunctionalityDescription
    = Function Text Synchronous
    -- ^ Exported function. Callable via @call name(arg1,arg2)@.
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Option to indicate how neovim should behave when calling this function

    | Command Text CommandOptions
    -- ^ Exported Command. Callable via @:Name arg1 arg2@.
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Options to configure neovim's behavior for calling the command

    | Autocmd Text Text AutocmdOptions
    -- ^ Exported autocommand. Will call the given function if the type and
    -- filter match.
    --
    -- NB: Since we are registering this on the Haskell side of things, the
    -- number of accepted arguments should be 0.
    -- TODO Should this be enforced somehow? Possibly via the TH generator.
    --
    -- * Type of the autocmd (e.g. \"BufWritePost\")
    -- * Name for the function to call

    deriving (Show, Read, Eq, Ord)


-- | This option detemines how neovim should behave when calling some
-- functionality on a remote host.
data Synchronous
    = Async
    -- ^ Call the functionality entirely for its side effects and do not wait
    -- for it to finish. Calling a functionality with this flag set is
    -- completely asynchronous and nothing is really expected to happen. This
    -- is why a call like this is called notification on the neovim side of
    -- things.

    | Sync
    -- ^ Call the function and wait for its result. This is only synchronous on
    -- the neovim side. For comands it means that the GUI will (probably) not
    -- allow any user input until a reult is received. Functions run
    -- asynchronously inside neovim (or in one of its plugin providers) can use
    -- these functions concurrently.
    deriving (Show, Read, Eq, Ord, Enum)


instance Default Synchronous where
    def = Sync

instance NvimObject Synchronous where
    toObject = \case
        Async -> toObject False
        Sync  -> toObject True

    fromObject = \case
        ObjectBool True  -> return Sync
        ObjectBool False -> return Async
        ObjectInt 0      -> return Async
        _                -> return Sync

data CommandOptions = CommandOptions
    { cmdSync  :: Synchronous
    -- ^ Option to indicate whether vim shuould block until the command has
    -- completed. (default: 'Sync')

    , cmdRange :: Maybe Text
    -- ^ Vim expression for the range (or count). (default: \"\")

    , cmdCount :: Bool
    -- ^ If true,

    , cmdNargs :: Int
    -- ^ Number of arguments. Note that all arguments have to be a string type.
    --
    -- TODO Check this in the Template Haskell functions.
    --
    -- If you're using the template haskell functions for registering commands,
    -- this field is overridden by it.

    , cmdBang  :: Bool
    -- ^ Behavior changes when using a bang.
    }
    deriving (Show, Read, Eq, Ord)


instance Default CommandOptions where
    def = CommandOptions
        { cmdSync  = Sync
        , cmdRange = Nothing
        , cmdCount = False
        , cmdNargs = 0
        , cmdBang  = False
        }


instance NvimObject CommandOptions where
    toObject (CommandOptions{..}) =
        (toObject :: Dictionary -> Object) . Map.fromList . catMaybes $
            [ cmdRange >>= \r -> Just ("range", toObject r)
            , if cmdCount then Just ("count", toObject True) else Nothing
            , if cmdNargs > 0 then Just ("nargs", toObject cmdNargs) else Nothing
            , if cmdBang then Just ("bang", toObject True) else Nothing
            ]
    fromObject o = throwError $
        "Did not expect to receive a CommandOptions object: " ++ show o


data AutocmdOptions = AutocmdOptions
    { acmdSync    :: Synchronous
    -- ^ Option to indicate whether vim shuould block until the function has
    -- completed. (default: 'Sync')

    , acmdPattern :: Text
    -- ^ Pattern to match on. (default: \"*\")

    , acmdNested  :: Bool
    -- ^ Nested autocmd. (default: False)
    --
    -- See @:h autocmd-nested@
    }
    deriving (Show, Read, Eq, Ord)


instance Default AutocmdOptions where
    def = AutocmdOptions
        { acmdSync    = Sync
        , acmdPattern = "*"
        , acmdNested  = False
        }


instance NvimObject AutocmdOptions where
    toObject (AutocmdOptions{..}) =
        (toObject :: Dictionary -> Object) . Map.fromList $
            [ ("pattern", toObject acmdPattern)
            , ("nested", toObject acmdNested)
            ]
    fromObject o = throwError $
        "Did not expect to receive an AutocmdOptions object: " ++ show o

-- | Conveniennce class to extract a name from some value.
class FunctionName a where
    name :: a -> Text


instance FunctionName FunctionalityDescription where
    name = \case
        Function  n _ -> n
        Command   n _ -> n
        Autocmd _ n _ -> n


instance FunctionName (ExportedFunctionality r st) where
    name = name . getDescription


-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { exports         :: [ExportedFunctionality () ()]
    , statefulExports :: [(r, st, [ExportedFunctionality r  st])]
    }


data NeovimPlugin = forall r st. NeovimPlugin (Plugin r st)


-- | Wrap a 'Plugin' in some nice blankets, so that we can put them in a simple
-- list.
wrapPlugin :: Monad m => Plugin r st -> m NeovimPlugin
wrapPlugin = return . NeovimPlugin

