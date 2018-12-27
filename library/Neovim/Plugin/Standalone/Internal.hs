{- |
Module      :  Neovim.Plugin.Standalone.Internal
Description :  Plugin to manage standalone plugins.
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Standalone.Internal
  ( nvimhsStartMeIfNecessary
  , StandaloneEnv(..)
  , StandaloneImplementation(..)
  ) where

import Neovim.Plugin.Standalone.Common

import Neovim.Context
import Neovim.Log

import           Control.Monad (when)
import qualified Data.Set      as Set
import           UnliftIO


logger :: String
logger = "Neovim.Plugin.Standalone.Internal"

-- | This function should be called with @expand('%:p')@ from a suitable vimL
-- file. Suitable means for example, that it is called from filetype-plugin
-- files if the plugin is only useful for these types of files.
nvimhsStartMeIfNecessary :: FilePath -> Maybe String -> StandalonePlugin ()
nvimhsStartMeIfNecessary fp exeName = do
  env <- ask
  isAlreadyStarted <- isStarted fp
  when (not isAlreadyStarted) $ do
    let is = preferredImplementations env
    cf <- findCabalFile fp
    atomically (shouldStart env fp cf) >>= \case
      Nothing -> return ()
      Just tMVarStarted -> do
        (exe, rpc) <- tryStart is cf (StandaloneExecutable <$> exeName)
        let host = (StartedPlugin exe rpc)
        isValidHost <- atomically $ tryPutTMVar tMVarStarted host
        registerHostRPC host
        when (not isValidHost) $
          liftIO $ errorM logger $ "Started the same plugin host multiple times"


tryStart
  :: [StandaloneImplementation]
  -> CabalFile
  -> Maybe StandaloneExecutable
  -> StandalonePlugin (StandaloneExecutable, RPCChannelId)
tryStart [] cf _ = throwString $ "Couldn't start plugin for cabal file: " ++ show cf
tryStart (i:is) cf mexe = do
    exes <- findValidExecutables i cf
    liftIO $ debugM logger $ "Found executables: " ++ show (Set.toList exes)
    case mexe of
      Nothing | Set.size exes == 1 ->
        tryBuildAndCompile (head $ Set.toList exes)
      Just exe | exe `Set.member` exes ->
        tryBuildAndCompile exe
      _ ->
        tryStart is cf mexe
  where
    tryBuildAndCompile exe =
      buildAndCompile i cf exe `catchAny` (\_ -> tryStart is cf mexe)


buildAndCompile
  :: StandaloneImplementation
  -> CabalFile
  -> StandaloneExecutable
  -> StandalonePlugin (StandaloneExecutable, RPCChannelId)
buildAndCompile i CabalFile{ projectDirectory = pwd } exe = do
  liftIO $ debugM logger $ "Building: " ++ show pwd
  build i pwd
  liftIO $ debugM logger $ unwords [ "Starting:", show pwd
                                   , "with executable",  show exe
                                   ]
  rpcChannelId <- start i pwd exe
  liftIO $ debugM logger $ unwords [ "Started:", show pwd
                                   , "with executable", show exe
                                   , "and rpc channel id:", show rpcChannelId
                                   ]
  return (exe, rpcChannelId)

-- -- | Scan neovim runtime path for standalone haskell projects.
-- scan :: StandalonePlugin [(FilePath, Set StandaloneExecutable)]
-- scan = do
--     possibleProjects <- nvim_list_runtime_paths'
--     implementations <- asks preferredImplementations
--     filter (not . Set.null . snd) <$> traverse (f implementations) possibleProjects
--   where
--     f :: [StandaloneImplementation]
--       -> FilePath
--       -> StandalonePlugin (FilePath, Set StandaloneExecutable)
--     f [] fp = return (fp, Set.empty)
--     f (i:is) fp = do
--       exes <- findValidExecutables i fp
--       if Set.null exes
--          then f is fp
--          else return (fp, exes)



