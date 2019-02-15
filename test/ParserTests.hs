module ParserTests (parserTests) where

import TestUtil
import Test.HUnit

import Parser
import ParserImpl
import ParseChar
import ParseNumber

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
        actual = evaluateParser letter "abc"

parseLetterFailure = assertBoolTest "parseLetterFailure" isLeft actual
  where actual = evaluateParser letter "123"

parseManyLettersEntireInput = assertEqTest "parseManyLettersEntireInput" expected actual
  where expected = Right ("", "abc")
        actual = evaluateParser (many letter) "abc"

parseManyLettersStopsCorrectly = assertEqTest "parseManyLettersStopsCorrectly" expected actual
  where expected = Right ("123", "abc")
        actual = evaluateParser (many letter) "abc123"

parseManyLettersWhenNone = assertEqTest "parseManyLettersWhenNone" expected actual
  where expected = Right ("123", "")
        actual = evaluateParser (many letter) "123"

parseManyLettersThenNumbers = assertEqTest "parseManyLettersThenNumbers" expected actual
  where expected = Right ("abc", ("abc", "123"))
        actual = evaluateParser parser "abc123abc"
        parser = (,) <$> many letter <*> many digit

parseIntSucceeds = assertEqTest "parseIntSucceeds" expected actual
  where expected = Right ("", 123)
        actual = evaluateParser int "123"

parseIntFails = assertBoolTest "parseIntFails" isLeft actual
  where actual = evaluateParser int "abc"
