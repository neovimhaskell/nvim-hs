{-# LANGUAGE OverloadedStrings #-}
module AsyncFunctionSpec where

import Neovim
import Neovim.API.Text
import Neovim.Plugin.Classes (FunctionName (..), FunctionalityDescription (..))
import Neovim.Plugin.Internal (ExportedFunctionality (..))
import Neovim.Test

import Test.Hspec

import UnliftIO

spec :: Spec
spec = do
    describe "an asynchronous function" $ do
        it "is callable" $ do
            called <- newEmptyMVar
            let myAsyncTestFunction = do
                    Plugin
                        { environment = ()
                        , exports =
                            [ EF
                                ( Function (F "MyAsyncTestFunction") Async
                                , \_args -> toObject <$> putMVar called ()
                                )
                            ]
                        }
            runInEmbeddedNeovim def{cancelAfter = Seconds 3} myAsyncTestFunction $ do
                void $ nvim_call_function "MyAsyncTestFunction" mempty

                liftIO $ readMVar called `shouldReturn` ()
