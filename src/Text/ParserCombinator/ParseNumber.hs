module Text.ParserCombinator.ParseNumber where

import Text.ParserCombinator.Parser
import Text.ParserCombinator.ParseChar
import Text.ParserCombinator.ParseGeneric

digits :: CharParser Int
digits = mread =<< some digit
