module Parser where

import Text.Parsec ( many1
                   , manyTill
                   , eof
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
import Text.Parsec.Char ( digit
                        , char
                        , string
                        , anyChar
                        , endOfLine
                        )
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
keyword s = void $ lexeme $ string s

symbol :: String -> Parser ()
symbol s = void $ lexeme $ string s

mod :: Parser GlobalStatement
mod = Module <$> (keyword "module" *> ident)

imprt :: Parser GlobalStatement
imprt = Import <$> (keyword "import" *> ident)

parameterList :: Parser [Parameter]
parameterList = sepBy (Parameter <$> ident <*> ident) (lexeme $ string ",")

function :: Parser GlobalStatement
function = Function <$> (keyword "def" *> ident) <*> (lexeme $ parens parameterList) <*> functionBody

globalStatement :: Parser GlobalStatement
globalStatement = Parser.mod <|> imprt <|> function

intLiteral :: Parser Expression
intLiteral = IntLiteral <$> many1 digit

expr :: Parser Expression
expr = intLiteral -- todo add more here

assignment :: Parser LocalStatement
assignment = Assignment <$> (keyword "let" *> ident <* char '=' <* whitespace) <*> expr

localStatement :: Parser LocalStatement
localStatement = lexeme $ assignment -- todo add more here

functionBody :: Parser [LocalStatement]
functionBody = many localStatement

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t\r "

whitespace1 :: Parser ()
whitespace1 = void $ many1 $ oneOf "\n\t\r "

comment :: Parser ()
--comment = void $ string "//" *> manyTill anyChar (void endOfLine <|> eof)
comment = void $ do
  string "//"
  manyTill anyChar (void endOfLine <|> eof)

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace <* many comment

ident :: Parser String
ident = lexeme $ do
  x <- firstChar
  xs <- many nonFirstChar
  return (x : xs)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parens :: Parser a -> Parser a
parens p = between (lexeme $ char '(') (lexeme $ char ')') p

