module Lex where

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
  | Print String
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
function = Function <$> (keyword "def" *> ident) <*> (parens parameterList) <*> pure []

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

