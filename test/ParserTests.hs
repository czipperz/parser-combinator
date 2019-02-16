module ParserTests (parserTests) where

import TestUtil
import Test.HUnit

import Lib
import ParserImpl

import Data.Either (isLeft)

parserTests = [testMakeSequence, parseLetterSuccessful, parseLetterFailure,
               parseManyLettersEntireInput, parseManyLettersStopsCorrectly,
               parseManyLettersWhenNone, parseManyLettersThenNumbers,
               parseIntSucceeds, parseIntFails]

testMakeSequence = assertTest "testMakeSequence" verify sequence
  where sequence = do
          a <- return (3 :: Int)
          b <- return 6
          Alternate (return a) (return b)
        verify (Alternate (Value x) (Value y)) = assertEqual "trySequence: Alternate matches" (3, 6) (x, y)
        verify _ = fail "trySequence: Not an alternate"

parseLetterSuccessful = assertEqTest "parseLetterSuccessful" expected actual
  where expected = Right ("bc", 'a')
        actual = parseRemainder letter "*test*" "abc"

parseLetterFailure = assertBoolTest "parseLetterFailure" isLeft actual
  where actual = parseRemainder letter "*test*" "123"

parseManyLettersEntireInput = assertEqTest "parseManyLettersEntireInput" expected actual
  where expected = Right ("", "abc")
        actual = parseRemainder (many letter) "*test*" "abc"

parseManyLettersStopsCorrectly = assertEqTest "parseManyLettersStopsCorrectly" expected actual
  where expected = Right ("123", "abc")
        actual = parseRemainder (many letter) "*test*" "abc123"

parseManyLettersWhenNone = assertEqTest "parseManyLettersWhenNone" expected actual
  where expected = Right ("123", "")
        actual = parseRemainder (many letter) "*test*" "123"

parseManyLettersThenNumbers = assertEqTest "parseManyLettersThenNumbers" expected actual
  where expected = Right ("abc", ("abc", "123"))
        actual = parseRemainder parser "*test*" "abc123abc"
        parser = (,) <$> many letter <*> many digit

parseIntSucceeds = assertEqTest "parseIntSucceeds" expected actual
  where expected = Right ("", 123)
        actual = parseRemainder int "*test*" "123"

parseIntFails = assertBoolTest "parseIntFails" isLeft actual
  where actual = parseRemainder int "*test*" "abc"
