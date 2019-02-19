module Text.ParserCombinator.ParseChar where

import Text.ParserCombinator.Parser
import Text.ParserCombinator.ParseGeneric
import Data.Char

control = charSatisfying "control" isControl
space = charSatisfying "space" isSpace
lower = charSatisfying "lower case letter" isLower
upper = charSatisfying "upper case letter" isUpper
alpha = charSatisfying "letter" isAlpha
alphaNum = charSatisfying "letter or number" isAlphaNum
printable = charSatisfying "print" isPrint
digit = charSatisfying "digit" isDigit
octDigit = charSatisfying "octal digit" isOctDigit
hexDigit = charSatisfying "hexadecimal digit" isHexDigit
letter = charSatisfying "letter" isLetter
mark = charSatisfying "mark" isMark
number = charSatisfying "number" isNumber
punctuation = charSatisfying "punctuation" isPunctuation
symbol = charSatisfying "symbol" isSymbol
separator = charSatisfying "separator" isSeparator

ascii = charSatisfying "ascii" isAscii
asciiLower = charSatisfying "ascii lower case" isAsciiLower
asciiUpper = charSatisfying "ascii upper case" isAsciiUpper
latin1 = charSatisfying "latin1" isLatin1

eol = setErrorMessage "end of line" (string "\n" <|> string "\r\n")

charSatisfying :: String -> (t -> Bool) -> Parser t t
charSatisfying = tokenSatisfying

string :: (Show t, Eq t) => [t] -> Parser t [t]
string = tokens

anyChar :: Parser t t
anyChar = anyToken
