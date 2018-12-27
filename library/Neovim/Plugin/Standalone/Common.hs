{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Neovim.Plugin.Standalone.Common
Description :  Common functionality
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Standalone.Common
    where

import Neovim.API.String
import Neovim.Classes
import Neovim.Context

import qualified Data.ByteString as BS
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import Distribution.PackageDescription.Parsec
    ( parseGenericPackageDescriptionMaybe
    )
import Distribution.Types.GenericPackageDescription
    ( GenericPackageDescription (condExecutables)
    )
import Distribution.Types.UnqualComponentName       (unUnqualComponentName)

import Control.Applicative
import Control.Monad             (void)
import Control.Monad.Trans.Maybe
import Data.List                 (isSuffixOf)
import Data.Maybe                (isJust, listToMaybe, mapMaybe)
import System.Directory          (doesDirectoryExist, listDirectory)
import System.FilePath           (takeDirectory, (</>))
import UnliftIO

import System.Exit          (ExitCode (ExitSuccess))
import System.Process.Typed


data RPCChannelId = RPCChannelId Word
  deriving (Eq, Ord, Show)

data StartedPlugin = StartedPlugin StandaloneExecutable RPCChannelId
  deriving (Eq, Ord, Show)


data StandaloneEnv = StandaloneEnv
  { startedPlugins           :: TVar (Map CabalFile (TMVar StartedPlugin))
  -- ^ Simple 'Map' from project root to one or more names of executables with
  -- their associated 'Process'.
  , preferredImplementations :: [StandaloneImplementation]
  -- ^ List of build tools to use in order to start a standalone plugin.
  , pathLookupCache          :: TVar (Map FilePath CabalFile)
  -- ^ Performance otpimization to avoid system calls and IO in general.
  }


type StandalonePlugin a = Neovim StandaloneEnv a


isStarted :: FilePath -> StandalonePlugin Bool
isStarted fp = do
  env <- ask
  atomically $ do
    cache <- readTVar (pathLookupCache env)
    startedPluginMap <- readTVar (startedPlugins env)
    mCabalFile <- runMaybeT $
                    lookupInCache cache <|> lookupInCacheIndirectly env cache
    return . isJust $ flip Map.lookup startedPluginMap =<< mCabalFile

  where
    lookupInCache :: Map FilePath CabalFile -> MaybeT STM CabalFile
    lookupInCache = MaybeT . return . Map.lookup fp

    lookupInCacheIndirectly env cache = MaybeT $
      case mapMaybe (flip Map.lookup cache) (thisAndParentDirectories fp) of
        [] -> return Nothing
        (cf:_) -> do
          modifyTVar' (pathLookupCache env) $ Map.insert fp cf
          return $ Just cf


shouldStart
  :: StandaloneEnv
  -> FilePath
  -> CabalFile
  -> STM (Maybe (TMVar StartedPlugin))
shouldStart StandaloneEnv{pathLookupCache,startedPlugins} fp cf =
  Map.lookup cf <$> readTVar startedPlugins >>= \case
    Just _ -> return Nothing
    Nothing -> do
      lock <- newEmptyTMVar
      modifyTVar' pathLookupCache $ Map.insert fp cf
      modifyTVar' startedPlugins $ Map.insert cf lock
      return $ Just lock


newtype StandaloneExecutable = StandaloneExecutable String
    deriving (Show, Eq, Ord)


jobStartRPC :: WorkingDirectory -> String -> [String] -> Neovim env RPCChannelId
jobStartRPC (WorkingDirectory pwd) cmd args = do
  channelObj <- nvim_call_function' "jobstart"
    $ (cmd:args)
    +: Map.fromList
        [ ("rpc" :: String, toObject True)
        , ("cwd"          , toObject pwd)
        ]
    +: []
  case fromObject channelObj of
    Right (r :: Int) | r > 0 ->
      return . RPCChannelId $ fromIntegral r
    _ ->
      err . pretty . unwords $ [ "Failed to initialize RPC process:", pwd
                               , " Command: " , cmd ] ++ args

registerHostRPC :: StartedPlugin -> Neovim env ()
registerHostRPC (StartedPlugin (StandaloneExecutable exe) (RPCChannelId c)) =
  void $ nvim_call_function' "remote#host#Register"
       $ exe +: ("*" :: String) +: c +: []

data CabalFile = CabalFile
  { projectDirectory :: WorkingDirectory
  , projectFile      :: FilePath
  } deriving (Eq, Ord, Show)

mkCabalFile :: (FilePath, FilePath) -> CabalFile
mkCabalFile (dir,file) = CabalFile (WorkingDirectory dir) file

absoluteCabalFilePath :: CabalFile -> FilePath
absoluteCabalFilePath CabalFile{projectDirectory = WorkingDirectory pwd, projectFile} =
  pwd </> projectFile


newtype WorkingDirectory = WorkingDirectory FilePath
    deriving (Show, Eq, Ord)


data StandaloneImplementation = StandaloneImplementation
  { build                :: WorkingDirectory -> StandalonePlugin ()
  -- ^ Given a project directory, build it.
  , start                :: WorkingDirectory
                         -> StandaloneExecutable
                         -> StandalonePlugin RPCChannelId
  -- ^ Run executable with the given working directory and name.
  , findValidExecutables :: CabalFile -> StandalonePlugin (Set StandaloneExecutable)
  -- ^ Check whether this implementation is suitable for the given directory and
  -- return runnable executable names.
  , onCrash              :: WorkingDirectory -> StandaloneExecutable -> StandalonePlugin ()
  -- ^ Recovery action to perform when plugin crashes.
  , isInstalled          :: StandalonePlugin Bool
  -- ^ Perform an action to test whether the implementation can be run.
  }


readExecutableNamesFromCabalFile
  :: MonadIO io
  => CabalFile -> io (Set StandaloneExecutable)
readExecutableNamesFromCabalFile cabalFile = liftIO $
    maybe mempty (convertToSet . extractExecutableNames) <$> readPackageDescription
  where
    readPackageDescription = parseGenericPackageDescriptionMaybe
                             <$> BS.readFile (absoluteCabalFilePath cabalFile)
    convertToSet = Set.fromList . map StandaloneExecutable
    extractExecutableNames = map (unUnqualComponentName . fst) . condExecutables


reportCrash :: WorkingDirectory -> StandaloneExecutable -> StandalonePlugin ()
reportCrash (WorkingDirectory pwd) (StandaloneExecutable exe) =
  err $ "Plugin crashed: " <> pretty pwd <+> pretty exe


thisAndParentDirectories :: FilePath -> [FilePath]
thisAndParentDirectories fp =
  let dirs = iterate takeDirectory fp
   in map fst . takeWhile (uncurry (/=)) $ zip dirs (tail dirs)


-- | Search in this and the parent directories for a cabal file and return the
-- the cabal file.
--
findCabalFile :: MonadIO io => FilePath -> io CabalFile
findCabalFile fp =
  maybe (throwString "No cabal file found") (return . mkCabalFile)
    =<< liftIO (findFile' (".cabal" `isSuffixOf`) fp)


-- | Similar to 'findFile' from "System.Directory", except that the given file
-- or directory name is expanded to all its parent directories and that the
-- predicate is the only file selection criteria.
findFile' :: (FilePath -> Bool) -> FilePath -> IO (Maybe (FilePath, FilePath))
findFile' p = go . thisAndParentDirectories
  where
    cabalFileInDirectory dir = doesDirectoryExist dir >>= \case
      False -> return Nothing
      True  -> listToMaybe . filter p <$> listDirectory dir
    go [] = return Nothing
    go (f:fs) = cabalFileInDirectory f >>= \case
                  Nothing -> go fs
                  Just cabalFile -> return $ Just (f, cabalFile)


-- | Test if the given command can be executed. The exit code of the command
-- must be 0 a.k.a. 'ExitSuccess'. If the executable does not exist or an
-- exception is thrown for some other reason, this function will return 'False'.
--
-- This is a useful function to implement 'isInstalled' for
-- 'StandaloneImplementation'.
canExecuteCommand :: MonadUnliftIO io => String -> [String] -> io Bool
canExecuteCommand cmd args =
  let testCommand = setStderr byteStringOutput
                  $ setStdout byteStringOutput
                  $ proc cmd args
      test = (ExitSuccess ==) <$> withProcess testCommand waitExitCode
   in liftIO test `catchAny` (\_ -> return False)

