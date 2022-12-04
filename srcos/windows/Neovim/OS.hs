module Neovim.OS (
  isWindows,
  getSocketUnix
) where
import Network.Socket (Socket)

isWindows :: Bool
isWindows = True

getSocketUnix :: FilePath -> IO Socket
getSocketUnix _ = fail "Windows' named pipes are no supported"
