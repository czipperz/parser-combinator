module Text.ParserCombinator.ParseGeneric where

import Text.ParserCombinator.Parser

-- | Consume the next token in the stream, failing on eoi.
anyToken :: Parser t t
anyToken = maybe (fail "End of input") return =<< consumeToken

satisfies :: (t -> Bool) -> Parser t t
satisfies = tokenSatisfying "did not satisfy predicate"

tokenSatisfying :: String -> (t -> Bool) -> Parser t t
tokenSatisfying e f = do
  t <- anyToken
  if f t
    then return t
    else fail e

eoi :: Parser t ()
eoi = maybe (return ()) (const $ fail "Not end of input") =<< consumeToken
