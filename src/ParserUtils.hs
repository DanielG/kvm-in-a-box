{-# LANGUAGE FlexibleContexts #-}
module ParserUtils where

import Data.Char
import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator

nonNewline :: Stream s m Char => ParsecT s u m Char
nonNewline      = satisfy (/='\n') <?> "non-newline"

nonNewlineSpace :: Stream s m Char => ParsecT s u m Char
nonNewlineSpace = satisfy (\c -> c /= '\n' && isSpace c) <?> "non-newline-space"

nnspaces :: Stream s m Char => ParsecT s u m ()
nnspaces = skipMany nonNewlineSpace
