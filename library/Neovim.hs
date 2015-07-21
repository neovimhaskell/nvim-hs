{- |
Module      :  Neovim
Description :  API for the neovim plugin provider /nvim-hs/
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC (due to Template Haskell)

This module should contain all the things you need to write neovim plugins in
your favorite language! @:-)@

The documentation in this module should enable you to write and debug plugins.
The chapters in this module start with a tl;dr paragraph that sums things up,
which is useful to get an idea whether you should actually read the chapter and
which will reduce your reading time if you just want to refresh your memory.
-}
module Neovim (
    -- * Architecture (optional)
    -- ** tl;dr Architecture
    -- $tldrarchitecture
    -- ** Under the hood
    -- $architecture
    ) where

import Neovim.Plugin.IPC.Internal (RPCMessage)

-- $tldrarchitecture
--
-- The general architecture is as asynchronous as it can get.
--
-- * "Neovim.RPC.SocketReader" is run as a thread. It reads msgpack-rpc objects
--    from @stdin@  and either spawns a thread which then executes the function,
--    passes the function arguments to a stateful plugin thread or it does
--    nothing.
--
-- * "Neovim.RPC.EventHandler" is run as a thread. It receives the results of
--   function calls as internal 'RPCMessage' values and writes them to @stdout@
--   according to the msgpack-rpc specification.
--
-- * Each __stateful plugin__ is run in a separate thread and shares its state
--    with all functions defined in the same context.
--
-- * Each invocation of a __stateless function__ spawns a small thread that
--   eventually returns.

-- $architecture
-- /nvim-hs/ is built around
