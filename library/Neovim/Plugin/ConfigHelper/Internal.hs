{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.Plugin.ConfigHelper.Internal
Description :  Internals for a config helper plugin that helps recompiling nvim-hs
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.ConfigHelper.Internal
    where

import           Neovim.API.String       (vim_command)
import           Neovim.Config
import           Neovim.Context
import           Neovim.Quickfix
import           Neovim.RPC.FunctionCall

import           Config.Dyre             (Params)
import           Config.Dyre.Compile
import           Control.Applicative     hiding (many, (<|>))
import           Control.Monad           (void)
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String

-- | Simple function that will return @"Pong"@ if the plugin provider is
-- running.
pingNvimhs :: Neovim' String
pingNvimhs = return "Pong"

-- | Recompile the plugin provider and put comile errors in the quickfix list.
recompileNvimhs :: Neovim (Params NeovimConfig) [QuickfixListItem String] ()
recompileNvimhs = do
    cfg <- ask
    mErrString <- liftIO (customCompile cfg >> getErrorString cfg)
    let qs = maybe [] parseQuickfixItems mErrString
    put qs
    setqflist qs Replace
    wait' $ vim_command "cwindow"

-- | Note that restarting the plugin provider implies compilation because Dyre
-- does this automatically. However, if the recompilation fails, the previously
-- compiled bynary is executed. This essentially means that restarting may take
-- more time then you might expect.
restartNvimhs :: Neovim r st ()
restartNvimhs = restart

-- See the tests in @test-suite\/Neovim\/Plugin\/ConfigHelperSpec.hs@ on how the
-- error messages look like.
parseQuickfixItems :: String -> [QuickfixListItem String]
parseQuickfixItems s =
    case parse (many pQuickfixListItem) "Quickfix parser" s of
        Right qs -> qs
        Left _   -> []

pQuickfixListItem :: Parser (QuickfixListItem String)
pQuickfixListItem = do
    _ <- many blankLine
    (f,l,c) <- pLocation
    desc <- try pShortDesrciption <|> pLongDescription
    return $ (quickfixListItem (Right f) (Left l))
        { col = Just (c, True)
        , text = desc
        , errorType = "E" -- TODO determine actual type
        }

pShortDesrciption :: Parser String
pShortDesrciption = (:)
    <$> (many spaceChar *> notFollowedBy blankLine *> anyChar)
    <*> anyChar `manyTill` (void (many1 blankLine) <|> eof)

pLongDescription :: Parser String
pLongDescription = anyChar `manyTill` (blank <|> eof)
  where
    blank = try (try newline *> try blankLine)

spaceChar :: Parser Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

blankLine :: Parser ()
blankLine = void . try $ many spaceChar >> newline

-- | Skip anything until the next location information appears.
--
-- The result will be a triple of filename, line number and column

-- | Try to parse location information.
--
-- @\/some\/path\/to\/a\/file.hs:42:88:@
pLocation :: Parser (String, Int, Int)
pLocation = (,,)
    <$> many1 (noneOf ":\n\t\r") <* char ':'
    <*> pInt <* char ':'
    <*> pInt <* char ':' <* many spaceChar

pInt :: Parser Int
pInt = read <$> many1 (satisfy isDigit)
