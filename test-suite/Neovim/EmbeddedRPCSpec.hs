{-# LANGUAGE DeriveDataTypeable #-}
module Neovim.EmbeddedRPCSpec
    where

import           Test.Hspec

import           Neovim
import           Neovim.RPC.Common
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader

import           Control.Concurrent
import           Control.Exception
import           Data.Data               (Typeable)
import qualified Data.Map                as Map
import           System.Directory
import           System.Process

data InterruptThread = InterruptThread
  deriving (Typeable, Show)

instance Exception InterruptThread

withNeovimEmbedded :: Maybe FilePath -> Neovim RPCConfig () a -> IO ()
withNeovimEmbedded file test = bracket
    startNvim
    killNvim
    (\(_, _, _, e) -> void $ runNeovim e () runTest)
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

        q <- newTQueueIO
        e <- ConfigWrapper q <$> newRPCConfig
        _ <- forkIO $ runSocketReader (Stdout hout) e
        _ <- forkIO $ runEventHandler (Stdout hin)  e

        return (hin, hout, ph, e)

    killNvim (_, _, ph, _) = do
        shouldKillNvim <- newTVarIO True
        _ <- forkIO $
            handle handleInterrupt $ do
                threadDelay $ 3 * 1000 * 1000 -- wait for 3 seconds
                shouldKill <- atomically' $ readTVar shouldKillNvim
                when shouldKill $ terminateProcess ph
        _ <- waitForProcess ph
        atomically' $ writeTVar shouldKillNvim False

    handleInterrupt InterruptThread = return ()

    runTest = do _ <- test
                 void $ vim_command "quit!"

spec :: Spec
spec = parallel $ do
  let helloFile = "test-files/hello"
  describe "Read hello test file" $
    it "should match 'Hello, World!'" $ withNeovimEmbedded (Just helloFile) $ do
        bs <- vim_get_buffers
        l <- vim_get_current_line
        liftIO $ l `shouldBe` Right "Hello, World!"
        liftIO $ length bs `shouldBe` 1

  describe "New empty buffer test" $ do
    it "should contain the test text" $ withNeovimEmbedded Nothing $ do
        cl0 <- vim_get_current_line
        liftIO $ cl0 `shouldBe` Right ""
        bs <- vim_get_buffers
        liftIO $ length bs `shouldBe` 1

        let testContent = "Test on empty buffer"
        wait' $ vim_set_current_line testContent
        cl1 <- vim_get_current_line
        liftIO $ cl1 `shouldBe` Right testContent
        recs <- atomically' . readTVar =<< asks recipients
        liftIO $ Map.size recs `shouldBe` 0

    it "should create a new buffer" $ withNeovimEmbedded Nothing $ do
        bs0 <- vim_get_buffers
        liftIO $ length bs0 `shouldBe` 1
        wait' $ vim_command "new"
        bs1 <- vim_get_buffers
        liftIO $ length bs1 `shouldBe` 2
        wait' $ vim_command "new"
        bs2 <- vim_get_buffers
        liftIO $ length bs2 `shouldBe` 3

        recs <- atomically' . readTVar =<< asks recipients
        liftIO $ Map.size recs `shouldBe` 0

