module Text.ParserCombinator.Parser (Parser, CharParser, consumeToken, setErrorMessage, many, some, (<|>)) where

import Text.ParserCombinator.ParserImpl
import Control.Applicative (many, some, (<|>))

type CharParser = Parser Char

-- | Consume the next token in the stream, if one exists.
consumeToken :: Parser t (Maybe t)
consumeToken = Consume id

setErrorMessage :: String -> Parser t a -> Parser t a
setErrorMessage = WithErrorMessage
