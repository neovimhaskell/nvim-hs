{-# LANGUAGE OverloadedStrings #-}
import           Neovim
import           Neovim.Main           (realMain)
import           Neovim.Plugin.Classes

import           Data.MessagePack
import           System.Log.Logger

-- The script `TestPlugins.vim` comments how these functions should behave.

main :: IO ()
main = realMain def
    { plugins = [ randPlugin ]
    }

randPlugin :: IO SomePlugin
randPlugin = do
    -- This plugin was intended to use a real random number generator, but
    -- unfortunately a Travis build with other GHC versions failed to reproduce
    -- the same numbers. So we just chose from these three numbers. You better
    -- don't use this for cryptography!
    let randomNumbers = cycle [42,17,-666] :: [Int16]
    return $ SomePlugin Plugin
      { exports = [Function "Randoom" randoom, Function "const42" const42]
      , statefulExports = [((), randomNumbers, [Function "Random" rf, Function "InjectNumber" inj])]
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
    Left _  -> return ObjectNil
inj _ = return ObjectNil

randoom :: [Object] -> Neovim cfg st Object
randoom _ = err "Function not supported"

const42 :: [Object] -> Neovim cfg st Object
const42 _ = return $ ObjectInt 42
