import Test.HUnit
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Monad (when)
import ParserTests
import BasicSchemeParserTests

tests = concat [parserTests, basicSchemeParserTests]

main :: IO ()
main = do
  counts <- runTestTT (TestList tests)
  let errs = errors counts + failures counts
  when (errs /= 0) $ exitWith (ExitFailure errs)
