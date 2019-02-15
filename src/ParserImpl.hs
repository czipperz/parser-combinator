{-# LANGUAGE ExistentialQuantification #-}

module ParserImpl where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)

data Parser a = Alternate (Parser a) (Parser a)
              | forall b. Sequence (Parser b) (b -> Parser a)
              | ConsumeChar (Maybe Char -> a)
              | Value a
              | Fail String

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return = Value
  fail = Fail

  Alternate l r >>= command = Alternate (l >>= command) (r >>= command)
  Sequence a b >>= command = Sequence a (fmap (>>= command) b)
  ConsumeChar f >>= command = Sequence (ConsumeChar f) command
  Value a >>= command = command a
  Fail s >>= _ = Fail s

instance Alternative Parser where
  empty = fail "Empty parser"
  (<|>) = Alternate

