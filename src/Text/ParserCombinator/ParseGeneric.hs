module Text.ParserCombinator.ParseGeneric where

import Text.ParserCombinator.Parser
import Text.Read (readEither)

-- | Consume the next token in the stream, failing on eoi.
anyToken :: Parser t t
anyToken = maybe (fail "End of input") return =<< maybeToken

satisfies :: (t -> Bool) -> Parser t t
satisfies = tokenSatisfying "did not satisfy predicate"

tokenSatisfying :: String -> (t -> Bool) -> Parser t t
tokenSatisfying e f = do
  t <- anyToken
  if f t
    then return t
    else fail e

tokens :: (Show t, Eq t) => [t] -> Parser t [t]
tokens xx = tokens' xx
  where tokens' [] = return []
        tokens' (x:xs) = do
          t <- anyToken
          if t == x
            then fmap (t:) (tokens' xs)
            else fail $ show xx

eoi :: Parser t ()
eoi = maybe (return ()) (const $ fail "Not end of input") =<< maybeToken

oneOf :: (Show t, Eq t) => [t] -> Parser t t
oneOf elems = tokenSatisfying ("one of " ++ show elems) (`elem` elems)

noneOf :: (Show t, Eq t) => [t] -> Parser t t
noneOf elems = tokenSatisfying ("none of " ++ show elems) (`notElem` elems)

mread :: (Monad m, Read a) => String -> m a
mread = meither . readEither

meither :: (Monad m) => Either String a -> m a
meither = either fail return
