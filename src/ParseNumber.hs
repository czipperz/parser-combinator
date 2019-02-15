module ParseNumber where

import Parser
import ParseChar

import Text.Read (readEither)

int :: Parser Int
int = mread =<< some digit

mread :: (Monad m, Read a) => String -> m a
mread = meither . readEither

meither :: (Monad m) => Either String a -> m a
meither = either fail return
