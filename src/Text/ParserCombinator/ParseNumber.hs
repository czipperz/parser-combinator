module Text.ParserCombinator.ParseNumber where

import Text.ParserCombinator.Parser
import Text.ParserCombinator.ParseChar

import Text.Read (readEither)

digits :: CharParser Int
digits = mread =<< some digit

mread :: (Monad m, Read a) => String -> m a
mread = meither . readEither

meither :: (Monad m) => Either String a -> m a
meither = either fail return
