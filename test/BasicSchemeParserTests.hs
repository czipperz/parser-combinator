module BasicSchemeParserTests (basicSchemeParserTests) where

import TestUtil
import Test.HUnit

import Text.ParserCombinator
import Text.ParserCombinator.ParserImpl
import Text.ParserCombinator.Pos

basicSchemeParserTests = [parseProgram1, parseProgram2, parseProgram3]

parseProgram1 = assertEqTest "parseProgram1" expected actual
  where expected = Right [Tag (Pos "*test*" 1 1) (Constant (Integer 13))]
        actual = parse program "*test*" "13"

parseProgram2 = assertEqTest "parseProgram2" expected actual
  where expected = Right [Tag (Pos "*test*" 1 1) (List [Tag (Pos "*test*" 1 2) (Constant (Boolean False))])]
        actual = parse program "*test*" "(#f)"

parseProgram3 = assertEqTest "parseProgram3" expected actual
  where expected = Left ""
        actual = parse program "*test*" "(#t13)"

------------------- PARSER -------------------

type Program = [Tag Expression]

data Expression =
  Constant Constant |
  Variable String |
  List [Tag Expression]
  deriving (Eq, Show)

data Constant =
  Boolean Bool |
  Integer Int |
  Character Char |
  String String
  deriving (Eq, Show)

program = do
  spaces
  exprs <- expressions
  eoi
  return exprs

expressions = noExpressions <|> headExpression
  where noExpressions = eoi >> return []
        headExpression = do
          expr <- expression
          spaces
          (expr:) <$> expressions

expression = named "expression" $ do
  spaces
  pos <- getPosition
  let pos' = Pos (sourceName pos) (sourceLine pos) (sourceColumn pos)
  Tag pos' <$> (constant <|> variable <|> list <?> "expression")

constant = Constant <$> (constantValue <?> "constant")
  where constantValue = boolean <|> number <|> character <|> stringConstant

boolean = t <|> f <?> "boolean"
  where t = string "#t" >> wordBoundary >> return (Boolean True)
        f = string "#f" >> wordBoundary >> return (Boolean False)

wordBoundary = do
  assertLookingAt "word boundary" $ eoi <|> ((char '(' <|> char ')' <|> space) >> return ()) <?> "word boundary"

number = integer <?> "number"
integer = Integer <$> value <?> "integer"
  where value = digits <|> do
          minus <- char '-'
          (\x -> -x) <$> digits
        digits = read <$> some digit

character = named "character" $ do
  string "#\\"
  Character <$> (space <|> newline <|> anyChar)
    where space = string "space" >> return ' '
          newline = string "newline" >> return '\n'

stringConstant = named "string" $ do
  char '"'
  contents <- many stringCharacter
  char '"'
  return . String . concat $ contents
    where stringCharacter = escapedQuote <|> escapedBackslash <|> normalStringCharacter
          escapedQuote = string "\\\"" >> return "\""
          escapedBackslash = string "\\\\" >> return "\\"
          normalStringCharacter = pure <$> noneOf ['\\', '"']

variable = Variable <$> identifier <?> "variable"
  where identifier = basicIdentifier <|> string "+" <|> string "-" <|> string "..." <?> "identifier"
        basicIdentifier = do
          initial <- initial
          subsequents <- many subsequent
          return (initial:subsequents)
        initial = letter <|> oneOf "!$%&*/:<=>?~_^"
        subsequent = initial <|> digit <|> oneOf ".+-"

list = named "list" $ do
  char '('
  List <$> (emptyList <|> listBody)
    where emptyList = do
            char ')' <?> "end of list"
            return []
          listBody = do
            expr <- expression
            spaces
            let endOfList = do
                  char ')' <?> "end of list"
                  return [expr]
            endOfList <|> ((expr:) <$> listBody)

