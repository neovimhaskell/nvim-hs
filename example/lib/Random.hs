{-# LANGUAGE TemplateHaskell #-}
module Random (plugin) where

import Neovim
import Random.Plugin (nextRandom, setNextRandom)
import System.Random (newStdGen, randoms)

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = do
    g <- liftIO newStdGen         -- initialize with a random seed
    let randomNumbers = randoms g -- an infinite list of random numbers
    wrapPlugin Plugin
        { exports         = []
        , statefulExports =
            [ ((), randomNumbers,
                [ $(function' 'nextRandom) Sync
                , $(function "SetNextRandom" 'setNextRandom) Async
                ])
            ]
        }
