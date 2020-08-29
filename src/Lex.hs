module Lex (lexBegin) where

import Control.Applicative
import Data.Char

data Token = IntegerLiteral Int
  | FloatLiteral Float
  | Identifier String
  | Plus
  | Minus
  | LeftSquare
  | RightSquare
  | LeftCurly
  | RightCurly
  | LeftParen
  | RightParen
  | Equals
  | DoubleQuote
  | Period
  | Comma deriving (Eq, Show)

lexInteger :: String -> Maybe (Token, String)
lexInteger input =
  let 
    numStr = takeWhile isNumber input
    len = length numStr
    remStr = drop len input
  in if len > 0
     then Just (IntegerLiteral $ read numStr, remStr)
     else Nothing

lexPlus :: String -> Maybe (Token, String)
lexPlus (x:xs) = if x == '+' then Just (Plus, xs) else Nothing

lexBegin :: String -> [Token]
lexBegin "" = []
lexBegin input =
  let res =   lexInteger input
          <|> lexPlus input
  in case res of
    Just (tok, rest) -> tok : lexBegin rest
    Nothing -> error $ "Could not lex '" ++ input ++ "'"
