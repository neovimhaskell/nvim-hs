{-# LANGUAGE LambdaCase #-}
module Neovim.RunNeovimWithTestPluginsSpec
    where

import           Test.Hspec
import           Test.HUnit         (assertFailure)

import           Control.Concurrent
import           System.Exit        (ExitCode (..))
import           System.IO          (hClose)
import           System.Process

spec :: Spec
spec = describe "The executable created by TestPlugins.hs should cause the\
      \ script TestPlugins.vim to return proper values." .
    it "should have an exit status of 0" $ do
      (_,Just o,_,ph) <- createProcess (proc "nvim" ["-n","-u","TestPlugins.vim"])
                         { std_out = CreatePipe }
      _ <- forkIO $ do
          threadDelay $ 10 * 1000 * 1000 -- 10 seconds
          getProcessExitCode ph >>= maybe (terminateProcess ph) (const $ return ())

      waitForProcess ph >>= \case
        ExitFailure i -> do
            hClose o
            assertFailure $ "Neovim returned with status code: " ++ show i

        _ ->
            hClose o


