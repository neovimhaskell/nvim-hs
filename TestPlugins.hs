{-# LANGUAGE OverloadedStrings #-}
import           Neovim
import           Neovim.API.Plugin
import           Neovim.Main       (realMain)

import           Data.MessagePack

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
      , statefulExports = [((), randomNumbers, [Function "Random" rf])]
      }

rf :: [Object] -> Neovim cfg [Int16] Object
rf _ = do
    r <- gets head
    modify tail
    return $ ObjectInt (fromIntegral r)

randoom :: [Object] -> Neovim cfg st Object
randoom _ = err "Function not supported"

const42 :: [Object] -> Neovim cfg st Object
const42 _ = return $ ObjectInt 42
