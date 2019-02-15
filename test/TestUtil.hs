module TestUtil where

import Test.HUnit

assertTest :: String -> (a -> Assertion) -> a -> Test
assertTest name func = TestLabel name . TestCase . func

assertBoolTest :: String -> (a -> Bool) -> a -> Test
assertBoolTest name test = assertTest name $ assertBool name . test
assertTrueTest :: String -> Bool -> Test
assertTrueTest name = assertBoolTest name id
assertFalseTest :: String -> Bool -> Test
assertFalseTest name = assertBoolTest name not

assertEqTest :: (Eq a, Show a) => String -> a -> a -> Test
assertEqTest name expected = assertTest name $ assertEqual name expected
