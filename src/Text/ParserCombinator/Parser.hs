module Text.ParserCombinator.Parser (Parser, CharParser, maybeToken, getPosition, (<?>), named,
                                     assertLookingAt, many, some, (<|>)) where

import Text.ParserCombinator.ParserImpl
import Text.ParserCombinator.Pos

import Control.Applicative (many, some, (<|>))

type CharParser = Parser Char

-- | Consume the next token in the stream, if one exists.
maybeToken :: Parser t (Maybe t)
maybeToken = Consume id

getPosition :: Parser t Pos
getPosition = GetPosition id

infixl 2 <?>
(<?>) :: Parser t a -> String -> Parser t a
(<?>) = flip WithErrorMessage

named :: String -> Parser t a -> Parser t a
named = flip (<?>)

assertLookingAt :: String -> Parser t a -> Parser t a
assertLookingAt s p = AssertLookingAt p <?> s
