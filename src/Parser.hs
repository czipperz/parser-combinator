{-# LANGUAGE ExistentialQuantification #-}

module Parser (Parser, CharParser, anyChar, consumeChar, many, some) where

import ParserImpl
import Control.Applicative (many, some)

type CharParser = Parser Char

-- | Consume the next char in the stream, failing on eoi.
anyChar :: CharParser Char
anyChar = maybe (fail "End of input") return =<< consumeChar

-- | Consume the next char in the stream, if one exists.
consumeChar :: CharParser (Maybe Char)
consumeChar = ConsumeChar id
