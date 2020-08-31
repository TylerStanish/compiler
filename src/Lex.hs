module Lex (lexBegin) where

import Control.Applicative
import Data.Char

data Token = IntegerLiteral Int
  | FloatLiteral Float
  | Identifier String
  | Plus
  | Minus
  | Star
  | Slash
  | LeftSquare
  | RightSquare
  | LeftCurly
  | RightCurly
  | LeftParen
  | RightParen
  | DoubleQuote
  | Period
  | Comma
  | Equals
  | EqualsEquals
  | Bang
  | BangEquals deriving (Eq, Show)

lexInteger :: String -> Maybe (Token, String)
lexInteger input =
  let 
    numStr = takeWhile isNumber input
    len = length numStr
    remStr = drop len input
  in if len > 0
     then Just (IntegerLiteral $ read numStr, remStr)
     else Nothing

lexFloat :: String -> Maybe (Token, String)
lexFloat input =
  let
    beforeDot = takeWhile (\c -> (isNumber c && (c /= '.'))) input
    inputAfterDot = drop (length beforeDot + 1) input
    afterDot = takeWhile (\c -> (isNumber c && (c /= '.'))) inputAfterDot
  in Nothing

lexChar :: Token -> Char -> String -> Maybe (Token, String)
lexChar tok c (x:xs) = if x == c then Just (tok, xs) else Nothing

lexBang :: String -> Maybe (Token, String)
lexBang (x:[]) = lexChar Bang '!' [x]
lexBang (x:y:xs) = case (x,y) of
                     ('!','=') -> Just (BangEquals, xs)
                     ('!',_) -> Just (Bang, y : xs)
                     _ -> Nothing

lexEquals :: String -> Maybe (Token, String)
lexEquals (x:[]) = lexChar Equals '=' [x]
lexEquals (x:y:xs) = case (x,y) of
                       ('=','=') -> Just (EqualsEquals, xs)
                       ('=',_) -> Just (Equals, y : xs)
                       _ -> Nothing
    

lexBegin :: String -> [Token]
lexBegin "" = []
lexBegin input =
  let res =   lexInteger input
          <|> lexChar Plus '+' input
          <|> lexChar Minus '-' input
          <|> lexChar Star '*' input
          <|> lexChar Slash '/' input
          <|> lexChar LeftSquare '[' input
          <|> lexChar RightSquare ']' input
          <|> lexChar LeftCurly '{' input
          <|> lexChar RightCurly '}' input
          <|> lexChar LeftParen '(' input
          <|> lexChar RightParen ')' input
          <|> lexChar DoubleQuote '"' input
          <|> lexChar Period '.' input
          <|> lexChar Comma ',' input
          <|> lexEquals input
          <|> lexBang input
  in case res of
    Just (tok, rest) -> tok : lexBegin rest
    Nothing -> error $ "Could not lex '" ++ input ++ "'"
