{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
import           Neovim
import           Neovim.Main (realMain)
import           Neovim.API.IPC
import           Neovim.API.Plugin

import           Data.Int          (Int16)
import           System.Log.Logger

-- The script `TestPlugins.vim` comments how these functions should behave.

main :: IO ()
main = realMain def
    { plugins = [ randPlugin ]
    }

randPlugin :: IO SomePlugin
randPlugin = do
    q <- newTQueueIO
    -- This plugin was intended to use a real random number generator, but
    -- unfortunately a Travis build with other GHC versions failed to reproduce
    -- the same numbers. So we just chose from these three numbers. You better
    -- don't use this for cryptography!
    let randomNumbers = cycle [42,17,-666] :: [Int16]
    return $ SomePlugin Plugin
      { name = "Random number generator"
      , functions = []
      , statefulFunctions = [("Random", q), ("Randoom", q)]
      , services = [ ( (), randomNumbers, nextRand q ) ]
      }

nextRand :: TQueue SomeMessage -> Neovim cfg [Int16] ()
nextRand q = do
    liftIO $ debugM "Random" "nextRand is running"
    Request{..} <- awaitRequest q
    liftIO $ debugM "Random" "nextRand got a request"
    -- We have registered two functions with the same TQueue, so we have to
    -- dispatch the function calls ourselves here.
    if | reqMethod == "Random" -> do
         r <- gets head
         modify tail
         respond reqId (Right (fromIntegral r :: Int64))
       | otherwise ->
           -- XXX Type signature needed for the type of the 'Right' result.
           let result = Left "Unexpected request received." :: Either String Int64
           in respond reqId result
    nextRand q
