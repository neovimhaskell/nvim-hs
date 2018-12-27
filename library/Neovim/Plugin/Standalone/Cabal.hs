{- |
Module      :  Neovim.Plugin.Standalone.Cabal
Description :  Cabal implementations for a standalone plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Standalone.Cabal
    where


import Neovim.Context
import Neovim.Plugin.Standalone.Common

import Data.Maybe           (isJust)
import System.Directory     (findExecutable)
import System.Process.Typed


v2Build :: MonadIO io => WorkingDirectory -> io ()
v2Build (WorkingDirectory pwd) =
  runProcess_ $ setWorkingDir pwd $ proc "cabal" ["v2-build"]


v2Exec
  :: WorkingDirectory -> StandaloneExecutable -> Neovim env RPCChannelId
v2Exec pwd (StandaloneExecutable exe) =
  jobStartRPC pwd "cabal" ["exec", exe, "--", exe, "-e"]


v2implementation :: StandaloneImplementation
v2implementation = StandaloneImplementation
  { build = v2Build
  , start = v2Exec
  , findValidExecutables = readExecutableNamesFromCabalFile
  , onCrash = reportCrash
  , isInstalled = liftIO $ isJust <$> findExecutable "cabal"
  }

