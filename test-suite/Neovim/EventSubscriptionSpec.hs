{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Neovim.EventSubscriptionSpec where

import Neovim
import Neovim.API.String
import Neovim.Test

import Test.Hspec

import UnliftIO (newEmptyMVar, putMVar, readMVar)

data BufLinesEvent = BufLinesEvent
    { bleBuffer :: Buffer
    , bleFirstLine :: Int
    , bleLastLine :: Int
    , bleLines :: [String]
    , bleMore :: Bool
    }
    deriving (Eq, Show)

parseBufLinesEvent :: [Object] -> Either (Doc AnsiStyle) BufLinesEvent
parseBufLinesEvent event = case event of
    [buf, _changedTick, firstline, lastline, linedata, more] -> do
        bleBuffer <- fromObject buf
        bleFirstLine <- fromObject firstline
        bleLastLine <- fromObject lastline
        bleLines <- fromObject linedata
        bleMore <- fromObject more
        pure BufLinesEvent{..}
    _ -> Left . pretty $ "Unexpected nvim_buf_lines_event: " ++ show event

spec :: Spec
spec = parallel $ do
    let withNeovimEmbedded = testWithEmbeddedNeovim Nothing (Seconds 2) ()
    describe "Attaching to a buffer" $ do
        it "receives nvim_buf_lines_event" . withNeovimEmbedded $ do
            received <- newEmptyMVar
            subscribe "nvim_buf_lines_event" $ putMVar received . parseBufLinesEvent
            buf <- nvim_create_buf True False
            isOk <- nvim_buf_attach buf True []

            liftIO $ do
                isOk `shouldBe` True
                Right BufLinesEvent{..} <- readMVar received
                bleBuffer `shouldBe` buf
                bleFirstLine `shouldBe` 0
                bleLastLine `shouldBe` -1
                bleLines `shouldBe` [""]
                bleMore `shouldBe` False

        it "receives nvim_buf_detach_event" . withNeovimEmbedded $ do
            received <- newEmptyMVar
            subscribe "nvim_buf_detach_event" $ putMVar received
            buf <- nvim_create_buf True False
            isOk <- nvim_buf_attach buf False []
            nvim_buf_detach buf

            liftIO $ do
                isOk `shouldBe` True
                [buf'] <- readMVar received
                fromObjectUnsafe buf' `shouldBe` buf
