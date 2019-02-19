module Text.ParserCombinator.Parse (parse, parseRemainder) where

import Text.ParserCombinator.ParserImpl
import Text.ParserCombinator.Pos

type FileName = String

parse :: Token t => Parser t a -> FileName -> [t] -> Either String a
parse fileName parser = fmap snd . parseRemainder fileName parser

parseRemainder :: Token t => Parser t a -> FileName -> [t] -> Either String ([t], a)
parseRemainder parser fileName = either (Left . mergeErrors) (return . snd) .
                                 parse' Nothing (initialPos fileName) parser

mergeErrors :: [Error] -> String
mergeErrors errors = "Input stalled as dead ends reached:" ++ errorStrings ++ "\n"
  where errorStrings = concatMap (('\n':) . displayTag) errors

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
parse' lastChar pos (Consume f) tt =
  Right ((t, maybe pos (incrementPos pos) lastChar),
         (ts, f t))
  where (t, ts) = if null tt
                  then (Nothing, [])
                  else (Just $ head tt, tail tt)
parse' lastChar pos (Value x) tt = Right ((lastChar, pos), (tt, x))
parse' _ pos (Fail e) _ = Left [Tag pos e]
parse' lastChar pos (WithErrorMessage s parser) tt = replaceErrorMessage $ parse' lastChar pos parser tt
  where replaceErrorMessage (Left _) = Left [Tag pos s]
        replaceErrorMessage r = r

class Token a where
  -- | Move the position from the beginning of the token to where the
  -- next token starts
  incrementPos :: Pos -> a -> Pos

instance Token Char where
  incrementPos = incrementPosChar
