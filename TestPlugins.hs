{-# LANGUAGE OverloadedStrings #-}
import           Neovim
import           Neovim.Main           (realMain)
import           Neovim.Plugin.Classes

import           System.Log.Logger

-- The script `TestPlugins.vim` comments how these functions should behave.

main :: IO ()
main = realMain def
    { plugins = [ randPlugin ]
    }

randPlugin :: IO NeovimPlugin
randPlugin = do
    -- This plugin was intended to use a real random number generator, but
    -- unfortunately a Travis build with other GHC versions failed to reproduce
    -- the same numbers. So we just chose from these three numbers. You better
    -- don't use this for cryptography!
    let randomNumbers = cycle [42,17,-666] :: [Int16]
    wrapPlugin Plugin
      { exports = [ EF (Function "Randoom" Sync, randoom)
                  , EF (Function "Const42" Sync, const42)
                  , EF (Function "PingNvimhs" Sync, pingNvimhs)
                  ]
      , statefulExports =
          [((), randomNumbers,
            [ EF (Function "Random" Sync, rf)
            , EF (Function "InjectNumber" Sync, inj)
            ])
          ]
      }

rf :: [Object] -> Neovim cfg [Int16] Object
rf _ = do
    r <- gets head
    modify tail
    return $ ObjectInt (fromIntegral r)

inj :: [Object] -> Neovim cfg [Int16] Object
inj [x] = case fromObject x of
    Right n -> do
        liftIO . debugM "TestPlugins.hs" $ "updating map"
        modify (n:)
        startofints <- gets (take 10)
        liftIO . debugM "TestPlugins.hs" $ "Updated map to: " <> show startofints
        return ObjectNil
    Left _  -> liftIO (debugM "TestPlugin.hs" ("wrong argument type " ++ show x)) >> return ObjectNil
inj x = liftIO (debugM "TestPlugin.hs" ("wrong argument form " ++ show x)) >> return ObjectNil

randoom :: [Object] -> Neovim cfg st Object
randoom _ = err "Function not supported"

const42 :: [Object] -> Neovim cfg st Object
const42 _ = return $ ObjectInt 42

pingNvimhs :: [Object] -> Neovim cfg st Object
pingNvimhs _ = return $ ObjectString "Pong"
