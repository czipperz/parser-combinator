module Parse (parse, parseRemainder) where

import ParserImpl
import Pos

type FileName = String

parse :: Token t => Parser t a -> FileName -> [t] -> Either String a
parse fileName parser = fmap snd . parseRemainder fileName parser

parseRemainder :: Token t => Parser t a -> FileName -> [t] -> Either String ([t], a)
parseRemainder parser fileName = either (Left . mergeErrors) (return . snd) .
                                 parse' Nothing (initialPos fileName) parser

mergeErrors :: [Error] -> String
mergeErrors _ = ""

type Error = Tag String

parse' :: Token t => Maybe t -> Pos -> Parser t a -> [t] -> Either [Error] ((Maybe t, Pos), ([t], a))
parse' lastChar pos (Alternate a b) tt =
  case parse' lastChar pos a tt of
    Right x -> Right x
    Left e -> case parse' lastChar pos b tt of
                Right x -> Right x
                Left e' -> Left (e ++ e')
parse' lastChar pos (Sequence a b) tt = do
  ((lastChar', pos'), (tt', x)) <- parse' lastChar pos a tt
  parse' lastChar' pos' (b x) tt'
parse' lastChar pos (Consume f) (t:ts) =
  Right ((Just t, maybe pos (incrementPos pos) lastChar),
         (ts, f $ Just t))
parse' _ pos (Consume _) [] = Left [Tag pos "End of input"]
parse' lastChar pos (Value x) tt = Right ((lastChar, pos), (tt, x))
parse' _ pos (Fail e) _ = Left [Tag pos e]

class Token a where
  -- | Move the position from the beginning of the token to where the
  -- next token starts
  incrementPos :: Pos -> a -> Pos

instance Token Char where
  incrementPos = incrementPosChar
