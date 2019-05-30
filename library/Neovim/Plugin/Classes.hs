{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
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
    FunctionalityDescription(..),
    FunctionName(..),
    NvimMethod(..),
    Synchronous(..),
    CommandOption(..),
    CommandOptions,
    RangeSpecification(..),
    CommandArguments(..),
    getCommandOptions,
    mkCommandOptions,
    AutocmdOptions(..),
    HasFunctionName(..),
    ) where

import           Neovim.Classes

import           Control.Applicative          hiding (empty)
import           Control.Monad.Error.Class
import           Data.ByteString              (ByteString)
import           Data.Char                    (isDigit)
import           Data.Default
import           Data.List                    (groupBy, sort)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.String
import           Data.Traversable             (sequence)
import           Data.Text.Encoding           (decodeUtf8)

import Data.Text.Prettyprint.Doc

import           Prelude                      hiding (sequence)


-- | Essentially just a string.
newtype FunctionName = F ByteString
    deriving (Eq, Ord, Show, Read, Generic)


instance NFData FunctionName


instance Pretty FunctionName where
    pretty (F n) = pretty $ decodeUtf8 n


-- | Functionality specific functional description entries.
--
-- All fields which are directly specified in these constructors are not
-- optional, but can partialy be generated via the Template Haskell functions.
-- The last field is a data type that contains all relevant options with
-- sensible defaults, hence 'def' can be used as an argument.
data FunctionalityDescription
    = Function FunctionName Synchronous
    -- ^ Exported function. Callable via @call name(arg1,arg2)@.
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Option to indicate how neovim should behave when calling this function

    | Command FunctionName CommandOptions
    -- ^ Exported Command. Callable via @:Name arg1 arg2@.
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Options to configure neovim's behavior for calling the command

    | Autocmd ByteString FunctionName Synchronous AutocmdOptions
    -- ^ Exported autocommand. Will call the given function if the type and
    -- filter match.
    --
    -- NB: Since we are registering this on the Haskell side of things, the
    -- number of accepted arguments should be 0.
    --
    -- * Type of the autocmd (e.g. \"BufWritePost\")
    -- * Name for the function to call
    -- * Whether to use rpcrequest or rpcnotify
    -- * Options for the autocmd (use 'def' here if you don't want to change anything)

    deriving (Show, Read, Eq, Ord, Generic)


instance NFData FunctionalityDescription


instance Pretty FunctionalityDescription where
    pretty = \case
        Function fname s ->
            "Function" <+> pretty s <+> pretty fname

        Command fname copts ->
            "Command" <+> pretty copts <+> pretty fname

        Autocmd t fname s aopts ->
            "Autocmd" <+> pretty (decodeUtf8 t)
                <+> pretty s
                <+> pretty aopts
                <+> pretty fname


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
    -- the neovim side. This means that the GUI will (probably) not
    -- allow any user input until a reult is received.
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance NFData Synchronous

instance Pretty Synchronous where
    pretty = \case
        Async -> "async"
        Sync  -> "sync"


instance IsString Synchronous where
    fromString = \case
        "sync"  -> Sync
        "async" -> Async
        _       -> error "Only \"sync\" and \"async\" are valid string representations"


instance NvimObject Synchronous where
    toObject = \case
        Async -> toObject False
        Sync  -> toObject True

    fromObject = \case
        ObjectBool True  -> return Sync
        ObjectBool False -> return Async
        ObjectInt 0      -> return Async
        _                -> return Sync


-- | Options for commands.
--
-- Some command can also be described by using the OverloadedString extensions.
-- This means that you can write a literal 'String' inside your source file in
-- place for a 'CommandOption' value. See the documentation for each value on
-- how these strings should look like (Both versions are compile time checked.)
data CommandOption = CmdSync Synchronous
                   -- ^ Stringliteral "sync" or "async"

                   | CmdRegister
                   -- ^ Register passed to the command.
                   --
                   -- Stringliteral: @\"\\\"\"@

                   | CmdNargs String
                   -- ^ Command takes a specific amount of arguments
                   --
                   -- Automatically set via template haskell functions. You
                   -- really shouldn't use this option yourself unless you have
                   -- to.

                   | CmdRange RangeSpecification
                   -- ^ Determines how neovim passes the range.
                   --
                   -- Stringliterals: \"%\" for 'WholeFile', \",\" for line
                   --                 and \",123\" for 123 lines.

                   | CmdCount Word
                   -- ^ Command handles a count. The argument defines the
                   -- default count.
                   --
                   -- Stringliteral: string of numbers (e.g. "132")

                   | CmdBang
                   -- ^ Command handles a bang
                   --
                   -- Stringliteral: \"!\"

    deriving (Eq, Ord, Show, Read, Generic)

instance NFData CommandOption


instance Pretty CommandOption where
    pretty = \case
        CmdSync s ->
            pretty s

        CmdRegister ->
            "\""

        CmdNargs n ->
            pretty n

        CmdRange rs ->
            pretty rs

        CmdCount c ->
            pretty c

        CmdBang ->
            "!"


instance IsString CommandOption where
    fromString = \case
        "%"     -> CmdRange WholeFile
        "\""    -> CmdRegister
        "!"     -> CmdBang
        "sync"  -> CmdSync Sync
        "async" -> CmdSync Async
        ","     -> CmdRange CurrentLine
        ',':ds | not (null ds) && all isDigit ds -> CmdRange (read ds)
        ds | not (null ds) && all isDigit ds -> CmdCount (read ds)
        _       -> error "Not a valid string for a CommandOptions. Check the docs!"

-- | Newtype wrapper for a list of 'CommandOption'. Any properly constructed
-- object of this type is sorted and only contains zero or one object for each
-- possible option.
newtype CommandOptions = CommandOptions { getCommandOptions :: [CommandOption] }
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData CommandOptions

instance Pretty CommandOptions where
    pretty (CommandOptions os) =
        cat $ map pretty os

-- | Smart constructor for 'CommandOptions'. This sorts the command options and
-- removes duplicate entries for semantically the same thing. Note that the
-- smallest option stays for whatever ordering is defined. It is best to simply
-- not define the same thing multiple times.
mkCommandOptions :: [CommandOption] -> CommandOptions
mkCommandOptions = CommandOptions . map head . groupBy constructor . sort
  where
    constructor a b = case (a,b) of
        _ | a == b               -> True
        -- Only CmdSync and CmdNargs may fail for the equality check,
        -- so we just have to check those.
        (CmdSync _, CmdSync _)         -> True
        (CmdRange _, CmdRange _)       -> True
        -- Range and conut are mutually recursive.
        -- XXX Actually '-range=N' and '-count=N' are, but the code in
        --     remote#define#CommandOnChannel treats it exclusive as a whole.
        --     (see :h :command-range)
        (CmdRange _, CmdCount _)       -> True
        (CmdNargs _, CmdNargs _)       -> True
        _                              -> False


instance NvimObject CommandOptions where
    toObject (CommandOptions opts) =
        (toObject :: Dictionary -> Object) . Map.fromList $ mapMaybe addOption opts
      where
        addOption = \case
            CmdRange r    -> Just ("range"   , toObject r)
            CmdCount n    -> Just ("count"   , toObject n)
            CmdBang       -> Just ("bang"    , ObjectBinary "")
            CmdRegister   -> Just ("register", ObjectBinary "")
            CmdNargs n    -> Just ("nargs"   , toObject n)
            _             -> Nothing

    fromObject o = throwError $
        "Did not expect to receive a CommandOptions object:" <+> viaShow o


-- | Specification of a range that acommand can operate on.
data RangeSpecification
    = CurrentLine
    -- ^ The line the cursor is at when the command is invoked.

    | WholeFile
    -- ^ Let the command operate on every line of the file.

    | RangeCount Int
    -- ^ Let the command operate on each line in the given range.

    deriving (Eq, Ord, Show, Read, Generic)

instance NFData RangeSpecification

instance Pretty RangeSpecification where
    pretty = \case
        CurrentLine ->
            mempty

        WholeFile ->
            "%"

        RangeCount c ->
            pretty c


instance NvimObject RangeSpecification where
    toObject = \case
        CurrentLine  -> ObjectBinary ""
        WholeFile    -> ObjectBinary "%"
        RangeCount n -> toObject n

    fromObject o = throwError $
      "Did not expect to receive a RangeSpecification object:" <+> viaShow o


-- | You can use this type as the first argument for a function which is
-- intended to be exported as a command. It holds information about the special
-- attributes a command can take.
data CommandArguments = CommandArguments
    { bang     :: Maybe Bool
    -- ^ 'Nothing' means that the function was not defined to handle a bang,
    -- otherwise it means that the bang was passed (@'Just' 'True'@) or that it
    -- was not passed when called (@'Just' 'False'@).

    , range    :: Maybe (Int, Int)
    -- ^ Range passed from neovim. Only set if 'CmdRange' was used in the export
    -- declaration of the command.
    --
    -- Example:
    --
    -- * @Just (1,12)@

    , count    :: Maybe Int
    -- ^ Count passed by neovim. Only set if 'CmdCount' was used in the export
    -- declaration of the command.

    , register :: Maybe String
    -- ^ Register that the command can\/should\/must use.
    }
    deriving (Eq, Ord, Show, Read, Generic)


instance NFData CommandArguments


instance Pretty CommandArguments where
    pretty CommandArguments{..} =
        cat $ catMaybes
            [ (\b -> if b then "!" else mempty) <$> bang
            , (\(s,e) -> lparen <> pretty s <> comma
                         <+> pretty e <> rparen)
                <$> range
            , pretty <$> count
            , pretty <$> register
            ]

instance Default CommandArguments where
    def = CommandArguments
            { bang     = Nothing
            , range    = Nothing
            , count    = Nothing
            , register = Nothing
            }


-- XXX This instance is used as a bit of a hack, so that I don't have to write
--     special code handling in the code generator and "Neovim.RPC.SocketReader".
instance NvimObject CommandArguments where
    toObject CommandArguments{..} = (toObject :: Dictionary -> Object)
        . Map.fromList . catMaybes $
            [ bang >>= \b -> return ("bang", toObject b)
            , range >>= \r -> return ("range", toObject r)
            , count >>= \c -> return ("count", toObject c)
            , register >>= \r -> return ("register", toObject r)
            ]

    fromObject (ObjectMap m) = do
        let l key = sequence (fromObject <$> Map.lookup (ObjectBinary key) m)
        bang <- l "bang"
        range <- l "range"
        count <- l "count"
        register <- l "register"
        return CommandArguments{..}

    fromObject ObjectNil = return def
    fromObject o =
        throwError $ "Expected a map for CommandArguments object, but got: "
                      <+> viaShow o


-- | Options that can be used to register an autocmd. See @:h :autocmd@ or any
-- referenced neovim help-page from the fields of this data type.
data AutocmdOptions = AutocmdOptions
    { acmdPattern :: String
    -- ^ Pattern to match on. (default: \"*\")

    , acmdNested  :: Bool
    -- ^ Nested autocmd. (default: False)
    --
    -- See @:h autocmd-nested@

    , acmdGroup   :: Maybe String
    -- ^ Group in which the autocmd should be registered.
    }
    deriving (Show, Read, Eq, Ord, Generic)


instance NFData AutocmdOptions


instance Pretty AutocmdOptions where
    pretty AutocmdOptions{..} =
        pretty acmdPattern
            <+> if acmdNested then "nested" else "unnested"
            <> maybe mempty (\g -> mempty <+> pretty g) acmdGroup


instance Default AutocmdOptions where
    def = AutocmdOptions
        { acmdPattern = "*"
        , acmdNested  = False
        , acmdGroup   = Nothing
        }


instance NvimObject AutocmdOptions where
    toObject AutocmdOptions{..} =
        (toObject :: Dictionary -> Object) . Map.fromList $
            [ ("pattern", toObject acmdPattern)
            , ("nested", toObject acmdNested)
            ] ++ catMaybes
            [ acmdGroup >>= \g -> return ("group", toObject g)
            ]
    fromObject o = throwError  $
        "Did not expect to receive an AutocmdOptions object: " <+> viaShow o

newtype NvimMethod =
  NvimMethod { nvimMethodName :: ByteString }
  deriving (Eq, Ord, Show, Read, Generic)


instance NFData NvimMethod


instance Pretty NvimMethod where
    pretty (NvimMethod n) = pretty $ decodeUtf8 n


-- | Conveniennce class to extract a name from some value.
class HasFunctionName a where
    name :: a -> FunctionName
    nvimMethod :: a -> NvimMethod


instance HasFunctionName FunctionalityDescription where
    name = \case
        Function  n _ -> n
        Command   n _ -> n
        Autocmd _ n _ _ -> n

    nvimMethod = \case
        Function (F n) _ -> NvimMethod $ "function:" <> n
        Command (F n) _ -> NvimMethod $ "command:" <> n
        Autocmd _ (F n) _ _ -> NvimMethod n
