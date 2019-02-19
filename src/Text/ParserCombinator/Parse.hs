module Text.ParserCombinator.Parse (parse, parseRemainder) where

import Text.ParserCombinator.ParserImpl
import Text.ParserCombinator.Pos

type FileName = String

parse :: Token t => Parser t a -> FileName -> [t] -> Either String a
parse fileName parser = fmap snd . parseRemainder fileName parser

parseRemainder :: Token t => Parser t a -> FileName -> [t] -> Either String ([t], a)
parseRemainder parser fileName = either (Left . mergeErrors) (return . snd) .
                                 parse' pos pos parser
  where pos = initialPos fileName

mergeErrors :: [Error] -> String
mergeErrors errors = "Input stalled as dead ends reached:" ++ errorStrings ++ "\n"
  where errorStrings = concatMap (('\n':) . displayTag) errors

type Error = Tag String

parse' :: Token t => Pos -> Pos -> Parser t a -> [t] -> Either [Error] ((Pos, Pos), ([t], a))
parse' previousPos pos (Alternate a b) tt =
  case parse' previousPos pos a tt of
    Right x -> Right x
    Left e -> case parse' previousPos pos b tt of
                Right x -> Right x
                Left e' -> Left (e ++ e')

parse' previousPos pos (Sequence a b) tt = do
  ((previousPos', pos'), (tt', x)) <- parse' previousPos pos a tt
  parse' previousPos' pos' (b x) tt'

parse' _ pos (Consume f) tt =
  Right ((pos, maybe pos (incrementPos pos) t),
         (ts, f t))
  where (t, ts) = if null tt
                  then (Nothing, [])
                  else (Just $ head tt, tail tt)

parse' previousPos pos (Value x) tt = Right ((previousPos, pos), (tt, x))

parse' _ pos (Fail e) _ = Left [Tag pos e]

parse' previousPos pos (WithErrorMessage s parser) tt =
  either (const $ Left [Tag pos s]) Right $ parse' previousPos pos parser tt

parse' previousPos pos (GetPosition f) tt =
  Right ((previousPos, pos), (tt, f pos))

class Token a where
  -- | Move the position from the beginning of the token to where the
  -- next token starts
  incrementPos :: Pos -> a -> Pos

instance Token Char where
  incrementPos = incrementPosChar
