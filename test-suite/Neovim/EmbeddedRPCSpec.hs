module Neovim.EmbeddedRPCSpec
    where

import           Test.Hspec

import           Neovim
import           Neovim.API.Context
import           Neovim.RPC.Common
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map                as Map
import           Data.Monoid
import           System.Directory
import           System.Process

withNeovimEmbedded :: Maybe FilePath -> Neovim () -> IO ()
withNeovimEmbedded file test = bracket
    startNvim
    killNvim
    (\(_, _, _, e) -> runNeovim e runTest)
  where
    startNvim = do
        args <- case file of
            Just f -> do
                fileExists <- doesFileExist f
                unless fileExists $ expectationFailure $ concat
                    ["File ", f, " does not exst."]
                return [f]
            Nothing -> return []
        (Just hin, Just hout, _, ph) <-
            createProcess (proc "nvim" (["-u","NONE","--embed"] <> args))
                { std_in = CreatePipe
                , std_out = CreatePipe
                }

        e <- newInternalEnvironment
        _ <- forkIO $ runSocketReader (Stdout hout) e
        _ <- forkIO $ runEventHandler (Stdout hin)  e

        return (hin, hout, ph, e)

    killNvim (_, _, ph, _) = do
        threadDelay $ 1000 * 1000 -- wait for a second
        terminateProcess ph

    runTest = do test
                 void $ vim_command "quit!"

spec :: Spec
spec = parallel $ do
  let helloFile = "test-files/hello"
  describe "Read hello test file" $
    it "should match 'Hello, World!'" $ withNeovimEmbedded (Just helloFile) $ do
        l <- vim_get_current_line
        liftIO $ l `shouldBe` Right "Hello, World!"
        bs <- vim_get_buffers
        liftIO $ length bs `shouldBe` 1

  describe "New empty buffer test" $ do
    it "should contain the test text" $ withNeovimEmbedded Nothing $ do
        cl0 <- vim_get_current_line
        liftIO $ cl0 `shouldBe` Right ""
        bs <- vim_get_buffers
        liftIO $ length bs `shouldBe` 1

        let testContent = "Test on empty buffer"
        _ <- atomically' =<< vim_set_current_line testContent
        cl1 <- vim_get_current_line
        liftIO $ cl1 `shouldBe` Right testContent
        recs <- atomically' . readTVar =<< asks recipients
        liftIO $ Map.size recs `shouldBe` 0

    it "should create a new buffer" $ withNeovimEmbedded Nothing $ do
        bs0 <- vim_get_buffers
        liftIO $ length bs0 `shouldBe` 1
        _ <- atomically' =<< vim_command "new"
        bs1 <- vim_get_buffers
        liftIO $ length bs1 `shouldBe` 2
        _ <- atomically' =<< vim_command "new"
        bs2 <- vim_get_buffers
        liftIO $ length bs2 `shouldBe` 3

        recs <- atomically' . readTVar =<< asks recipients
        liftIO $ Map.size recs `shouldBe` 0

