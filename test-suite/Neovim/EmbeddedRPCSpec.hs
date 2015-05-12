{-# LANGUAGE LambdaCase #-}
module Neovim.EmbeddedRPCSpec
    where

import           Test.Hspec
import           Test.HUnit

import           Neovim
import           Neovim.RPC.Common
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader

import           Control.Concurrent
import qualified Data.Map                as Map
import           System.Directory
import           System.Exit             (ExitCode (..))
import           System.Process

withNeovimEmbedded :: Maybe FilePath -> Neovim RPCConfig () a -> Assertion
withNeovimEmbedded file test = do
    (_, _, ph, e) <- startNvim
    void $ runNeovim e () runTest
    waitForProcess ph >>= \case
      ExitFailure i -> assertFailure $ "Neovim returned " ++ show i
      ExitSuccess   -> return ()
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

        -- We give the test 3 seconds
        -- TODO: Maybe make timeout a function parameter
        _ <- forkIO $ do
               threadDelay $ 3 * 1000 * 1000
               getProcessExitCode ph >>= maybe (terminateProcess ph) (const $ return ())

        return (hin, hout, ph, e)

    runTest = do _ <- test
                 void $ vim_command "qa!"

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

