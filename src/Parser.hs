{-# LANGUAGE ExistentialQuantification #-}

module Parser (Parser, CharParser, anyChar, consumeChar, evaluateParser, many, some) where

import ParserImpl
import Control.Applicative (many, some)

type CharParser = Parser Char

-- | Consume the next char in the stream, failing on eoi.
anyChar :: CharParser Char
anyChar = maybe (fail "End of input") return =<< consumeChar

-- | Consume the next char in the stream, if one exists.
consumeChar :: CharParser (Maybe Char)
consumeChar = ConsumeChar id

evaluateParser :: CharParser a -> String -> Either String (String, a)
evaluateParser (Alternate a b) xx =
  case evaluateParser a xx of
    Right x -> Right x
    Left e -> evaluateParser b xx
evaluateParser (Sequence a b) xx = do
  (xx', x) <- evaluateParser a xx
  evaluateParser (b x) xx'
evaluateParser (ConsumeChar f) (x:xs) = Right (xs, f $ Just x)
evaluateParser (ConsumeChar _) [] = Left "End of input"
evaluateParser (Value x) xx = Right (xx, x)
evaluateParser (Fail e) _ = Left e
