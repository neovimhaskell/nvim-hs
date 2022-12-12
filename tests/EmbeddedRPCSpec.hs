{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EmbeddedRPCSpec where

import Test.Hspec

import Neovim
import Neovim.API.Text
import Neovim.Context (docToText)
import Neovim.Quickfix
import Neovim.Test

{- | Tests in here should always be wrapped in 'runInEmbeddedNeovim' def' because they
 don't fail if neovim isn't installed.  This is particularly helpful to run
 tests on stackage and be notified if non-neovim-dependent tests fail.
 Basically everybody else who runs these tests has neovim installed and would
 see the test failing.
-}
spec :: Spec
spec = parallel $ do
    describe "Read hello test file" $
        it "should match 'Hello, World!'" . runInEmbeddedNeovim' def $ do
            nvim_command "edit test-files/hello"
            bs <- vim_get_buffers
            l <- vim_get_current_line
            liftIO $ l `shouldBe` "Hello, World!"
            liftIO $ length bs `shouldBe` 1

    describe "New empty buffer test" $ do
        it "should contain the test text" . runInEmbeddedNeovim' def $ do
            cl0 <- vim_get_current_line
            liftIO $ cl0 `shouldBe` ""
            bs <- vim_get_buffers
            liftIO $ length bs `shouldBe` 1

            let testContent = "Test on empty buffer"
            vim_set_current_line testContent
            cl1 <- vim_get_current_line
            liftIO $ cl1 `shouldBe` testContent

        it "should create a new buffer" . runInEmbeddedNeovim' def $ do
            bs0 <- vim_get_buffers
            liftIO $ length bs0 `shouldBe` 1
            vim_command "new"
            bs1 <- vim_get_buffers
            liftIO $ length bs1 `shouldBe` 2
            vim_command "new"
            bs2 <- vim_get_buffers
            liftIO $ length bs2 `shouldBe` 3

        it "should set the quickfix list" . runInEmbeddedNeovim' def $ do
            let q = quickfixListItem (Left 1) (Left 0) :: QuickfixListItem String
            setqflist [q] Replace
            q' <- vim_eval "getqflist()"
            liftIO $ fromObjectUnsafe q' `shouldBe` [q]

        it "throws NeovimException with function that failed as Doc" . runInEmbeddedNeovim' def $ do
            let getVariableValue = False <$ vim_get_var "notDefined"
            hasTrhownNeovimExceptionWithFunctionName <-
                getVariableValue `catchNeovimException` \case
                    ErrorResult f _ -> pure $ docToText f == "vim_get_var"
                    _ -> pure False
            liftIO $ hasTrhownNeovimExceptionWithFunctionName `shouldBe` True

        it "catches" . runInEmbeddedNeovim' def $ do
            let getUndefinedVariable = vim_get_var "notDefined"
            functionThatFailed <-
                getUndefinedVariable `catchNeovimException` \case
                    ErrorResult f _ -> pure . toObject $ docToText f
                    _ -> pure ObjectNil
            liftIO $ functionThatFailed `shouldBe` toObject ("vim_get_var" :: String)
