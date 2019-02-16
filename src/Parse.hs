module Parse where

import ParserImpl

parse :: Parser s a -> [s] -> Either String ([s], a)
parse (Alternate a b) xx =
  case parse a xx of
    Right x -> Right x
    Left e -> parse b xx
parse (Sequence a b) xx = do
  (xx', x) <- parse a xx
  parse (b x) xx'
parse (ConsumeChar f) (x:xs) = Right (xs, f $ Just x)
parse (ConsumeChar _) [] = Left "End of input"
parse (Value x) xx = Right (xx, x)
parse (Fail e) _ = Left e
