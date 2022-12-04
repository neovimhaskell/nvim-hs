module Neovim.OS (
  isWindows,
  getSocketUnix
) where

import Data.Streaming.Network (getSocketUnix)

isWindows :: Bool
isWindows = False

