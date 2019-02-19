module Text.ParserCombinator.Parser (Parser, CharParser, maybeToken, (<?>), many, some, (<|>)) where

import Text.ParserCombinator.ParserImpl
import Control.Applicative (many, some, (<|>))

type CharParser = Parser Char

-- | Consume the next token in the stream, if one exists.
maybeToken :: Parser t (Maybe t)
maybeToken = Consume id

infixl 2 <?>
(<?>) :: Parser t a -> String -> Parser t a
(<?>) = flip WithErrorMessage
