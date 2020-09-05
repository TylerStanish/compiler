module Parser where

import Text.Parsec ( many1
                   , runParserT
                   , parseTest
                   , runParser
                   , parse
                   , ParseError
                   , satisfy
                   , oneOf
                   , choice
                   , between
                   , sepBy
                   )
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

data Expression
  = IntLiteral String
  | StringLiteral String
  | Addition String String
  deriving (Eq, Show)

data Parameter = Parameter String String deriving (Eq, Show)

data GlobalStatement
  = Module String
  | Import String
  | Function String [Parameter] [LocalStatement]
  deriving (Eq, Show)

data LocalStatement
  = Assignment String Expression
  deriving (Eq, Show)

run :: Parser a -> String -> Either ParseError a
run parser input = (parse parser) "" input

keyword :: String -> Parser ()
keyword s = string s *> whitespace1

mod :: Parser GlobalStatement
mod = Module <$> (keyword "module" *> ident)

imprt :: Parser GlobalStatement
imprt = Import <$> (keyword "import" *> ident)

parameterList :: Parser [Parameter]
parameterList = sepBy (Parameter <$> ident <*> ident) (string "," <* whitespace)

function :: Parser GlobalStatement
function = Function <$> (keyword "def" *> ident) <*> (parens parameterList) <*> functionBody

globalStatement :: Parser GlobalStatement
globalStatement = Parser.mod <|> imprt <|> function

intLiteral :: Parser Expression
intLiteral = IntLiteral <$> many1 digit

expr :: Parser Expression
expr = intLiteral -- todo add more here

assignment :: Parser LocalStatement
assignment = Assignment <$> (keyword "let" *> ident <* whitespace <* char '=' <* whitespace) <*> expr

localStatement :: Parser LocalStatement
localStatement = assignment -- todo add more here

functionBody :: Parser [LocalStatement]
functionBody = many localStatement

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t\r "

whitespace1 :: Parser ()
whitespace1 = void $ many1 $ oneOf "\n\t\r "

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

ident :: Parser String
ident = do
  x <- firstChar
  xs <- many nonFirstChar
  whitespace
  return (x : xs)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

