{-# LANGUAGE LambdaCase #-}
module Neovim.EmbeddedRPCSpec
    where

import           Test.Hspec
import           Test.HUnit

import           Neovim
import           Neovim.Test
import qualified Neovim.Context.Internal      as Internal
import           Neovim.Quickfix
import           Neovim.RPC.Common
import           Neovim.RPC.EventHandler
import           Neovim.RPC.FunctionCall      (atomically')
import           Neovim.RPC.SocketReader

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.State          (runStateT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map                     as Map
import           System.Directory
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hClose)
import           System.Process


spec :: Spec
spec = parallel $ do
  let helloFile = "test-files/hello"
      withNeovimEmbedded f a = testWithEmbeddedNeovim f 3 () () a
  describe "Read hello test file" .
    it "should match 'Hello, World!'" . withNeovimEmbedded (Just helloFile) $ do
        bs <- vim_get_buffers
        Right l <- vim_get_current_line
        liftIO $ l `shouldBe` "Hello, World!"
        liftIO $ length bs `shouldBe` 1

  describe "New empty buffer test" $ do
    it "should contain the test text" . withNeovimEmbedded Nothing $ do
        Right cl0 <- vim_get_current_line
        liftIO $ cl0 `shouldBe` ""
        bs <- vim_get_buffers
        liftIO $ length bs `shouldBe` 1

        let testContent = "Test on empty buffer"
        vim_set_current_line testContent
        Right cl1 <- vim_get_current_line
        liftIO $ cl1 `shouldBe` testContent

    it "should create a new buffer" . withNeovimEmbedded Nothing $ do
        Right bs0 <- vim_get_buffers
        liftIO $ length bs0 `shouldBe` 1
        vim_command "new"
        Right bs1 <- vim_get_buffers
        liftIO $ length bs1 `shouldBe` 2
        vim_command "new"
        Right bs2 <- vim_get_buffers
        liftIO $ length bs2 `shouldBe` 3

    it "should set the quickfix list" . withNeovimEmbedded Nothing $ do
        let q = quickfixListItem (Left 1) (Left 0) :: QuickfixListItem String
        setqflist [q] Replace
        Right q' <- vim_eval "getqflist()"
        liftIO $ fromObjectUnsafe q' `shouldBe` [q]

