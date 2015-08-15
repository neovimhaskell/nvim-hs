{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Neovim
import qualified Neovim.Context.Internal   as Internal
import           Neovim.Main               (realMain)
import           Neovim.Plugin.Classes
import           TestPlugins.TestFunctions

import           Control.Concurrent        (readMVar)

-- The script `TestPlugins.vim` comments how these functions should behave.

main :: IO ()
main = realMain finalizer def
    { plugins = [ randPlugin ]
    }

finalizer tids cfg = readMVar (Internal.quit cfg) >>= \case
    Internal.InitSuccess ->
        finalizer tids cfg

    _ ->
        return ()

randPlugin :: Neovim' NeovimPlugin
randPlugin = liftIO $ do
    -- This plugin was intended to use a real random number generator, but
    -- unfortunately a Travis build with other GHC versions failed to reproduce
    -- the same numbers. So we just chose from these three numbers. You better
    -- don't use this for cryptography!
    let randomNumbers = cycle [42,17,-666] :: [Int16]
    wrapPlugin Plugin
      { exports = [ $(function' 'randoom) Sync
                  , $(function' 'const42) Sync
                  , $(function "PingNvimhs" 'pingNvimhs) Sync
                  , $(command "ComplicatedSpecialArgsHandling" 'complicatedCommand)
                      [ CmdSync Sync, CmdRange WholeFile
                      , CmdBang , CmdNargs "+"
                      ]
                  ]
      , statefulExports =
          [((), randomNumbers,
            [ $(function "Random" 'rf) Sync
            , $(function "InjectNumber" 'inj) Sync
            , $(function "InitLazy" 'registerFunctionLazily) Sync
            ])
          ]
      }

