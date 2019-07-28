{-# LANGUAGE ExistentialQuantification #-}

module Text.ParserCombinator.ParserImpl where

import Text.ParserCombinator.Pos

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)

data Parser t a = Alternate (Parser t a) (Parser t a)
                | forall b. Sequence (Parser t b) (b -> Parser t a)
                | Consume (Maybe t -> a)
                | Value a
                | Fail String
                | WithErrorMessage String (Parser t a)
                | GetPosition (Pos -> a)
                | AssertLookingAt (Parser t a)

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
  Consume f >>= command = Sequence (Consume f) command
  Value a >>= command = command a
  Fail s >>= _ = Fail s
  WithErrorMessage s p >>= f = WithErrorMessage s (p >>= f)
  GetPosition f >>= command = Sequence (GetPosition f) command
  AssertLookingAt p >>= command = Sequence (AssertLookingAt p) command

instance Alternative (Parser t) where
  empty = fail "Empty parser"
  (<|>) = Alternate
