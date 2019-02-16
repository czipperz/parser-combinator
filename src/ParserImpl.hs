{-# LANGUAGE ExistentialQuantification #-}

module ParserImpl where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)

data Parser s a = Alternate (Parser s a) (Parser s a)
                | forall b. Sequence (Parser s b) (b -> Parser s a)
                | ConsumeChar (Maybe s -> a)
                | Value a
                | Fail String

instance Functor (Parser s) where
  fmap = liftM

instance Applicative (Parser s) where
  pure = return
  (<*>) = ap

instance Monad (Parser s) where
  return = Value
  fail = Fail

  Alternate l r >>= command = Alternate (l >>= command) (r >>= command)
  Sequence a b >>= command = Sequence a (fmap (>>= command) b)
  ConsumeChar f >>= command = Sequence (ConsumeChar f) command
  Value a >>= command = command a
  Fail s >>= _ = Fail s

instance Alternative (Parser s) where
  empty = fail "Empty parser"
  (<|>) = Alternate
