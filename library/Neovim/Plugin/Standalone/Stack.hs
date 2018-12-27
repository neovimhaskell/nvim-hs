{- |
Module      :  Neovim.Plugin.Standalone.Stack
Description :  Stack implementation for a standalone plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Standalone.Stack
  where


import Neovim.Context
import Neovim.Plugin.Standalone.Common

import Data.Maybe           (isJust)
import Data.Set             (Set)
import System.Directory     (findExecutable, findFile)
import System.Exit          (ExitCode)
import System.Process.Typed
import UnliftIO


hasStackConfigFile :: MonadIO io => WorkingDirectory -> io Bool
hasStackConfigFile (WorkingDirectory pwd) =
  isJust <$> liftIO (findFile [pwd] "stack.yaml")


stackFindValidExecutables :: MonadIO io => CabalFile -> io (Set StandaloneExecutable)
stackFindValidExecutables cabalFile =
  hasStackConfigFile (projectDirectory cabalFile) >>= \case
    True -> readExecutableNamesFromCabalFile cabalFile
    False -> return mempty


stackBuild :: MonadIO io => WorkingDirectory -> io ()
stackBuild (WorkingDirectory pwd) =
  runProcess_ $ setWorkingDir pwd $ proc "stack" ["build"]


stackStart
  :: WorkingDirectory -> StandaloneExecutable -> Neovim env RPCChannelId
stackStart pwd (StandaloneExecutable exe) = do
  jobStartRPC pwd "stack" [ "exec", exe, "--", exe, "-e" ]


implementation :: StandaloneImplementation
implementation = StandaloneImplementation
  { build = stackBuild
  , start = stackStart
  , findValidExecutables = stackFindValidExecutables
  , onCrash = reportCrash
  , isInstalled = liftIO $ isJust <$> findExecutable "stack"
  }

