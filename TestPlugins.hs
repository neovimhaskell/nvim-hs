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
import           System.Random

-- The script `TestPlugins.vim` comments how these functions should behave.

main :: IO ()
main = realMain def
    { plugins = [ randPlugin ]
    }

randPlugin :: IO SomePlugin
randPlugin = do
    q <- newTQueueIO
    -- g <- newStdGen -- A real random number generator would do this
    let g = mkStdGen 42
    return $ SomePlugin Plugin
      { name = "Random number generator"
      , functions = []
      , statefulFunctions = [("Random", q), ("Randoom", q)]
      , services = [ ( (), g, nextRand q ) ]
      }

nextRand :: RandomGen s => TQueue SomeMessage -> Neovim cfg s ()
nextRand q = do
    liftIO $ debugM "Random" "nextRand is running"
    Request{..} <- awaitRequest q
    liftIO $ debugM "Random" "nextRand got a request"
    if | reqMethod == "Random" -> do
         (r,g) <- random <$> get
         let r' = r :: Int16
         put g
         respond reqId (Right (fromIntegral r' :: Int64))
       | otherwise ->
           -- XXX Type signature needed for the type of the 'Right' result.
           let result = Left "Unexpected request received." :: Either String Int64
           in respond reqId result
    nextRand q
