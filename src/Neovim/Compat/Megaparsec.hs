{-# LANGUAGE CPP #-}
module Neovim.Compat.Megaparsec
    ( Parser
    , module X
#if MIN_VERSION_megaparsec(7,0,0)
    , anyChar
#endif
    ) where


import Text.Megaparsec as X

#if MIN_VERSION_megaparsec(6,0,0)

import           Data.Void
import           Text.Megaparsec.Char as X

type Parser = Parsec Void String

#else

import           Text.Megaparsec.String as X

#endif

#if MIN_VERSION_megaparsec(7,0,0)
anyChar :: Parser Char
anyChar = anySingle
#endif

