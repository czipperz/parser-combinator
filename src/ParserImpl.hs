{-# LANGUAGE ExistentialQuantification #-}

module ParserImpl where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)

data Parser t a = Alternate (Parser t a) (Parser t a)
                | forall b. Sequence (Parser t b) (b -> Parser t a)
                | ConsumeChar (Maybe t -> a)
                | Value a
                | Fail String

instance Functor (Parser t) where
  fmap = liftM

instance Applicative (Parser t) where
  pure = return
  (<*>) = ap

instance Monad (Parser t) where
  return = Value
  fail = Fail

  Alternate l r >>= command = Alternate (l >>= command) (r >>= command)
  Sequence a b >>= command = Sequence a (fmap (>>= command) b)
  ConsumeChar f >>= command = Sequence (ConsumeChar f) command
  Value a >>= command = command a
  Fail s >>= _ = Fail s

instance Alternative (Parser t) where
  empty = fail "Empty parser"
  (<|>) = Alternate
