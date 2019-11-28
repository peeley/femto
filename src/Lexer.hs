{-# LANGUAGE LambdaCase #-}
module Lexer where

import ParserType
import Control.Applicative
import Control.Monad (void)

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = Parser $ \case 
    [] -> Nothing
    (x:xs) -> if f x then Just (x, xs)
              else Nothing

char :: Char -> Parser Char
char x = satisfies (x ==)

oneOf :: String -> Parser Char
oneOf chars = satisfies (`elem` chars)

letter :: Parser Char
letter = oneOf "ABCDEFGHIJKLMNOPQKRSTUIVQXYZabcdefghijklmnopqrstuvwxyz"

digit :: Parser Char
digit = oneOf "1234567890"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

skipMany :: Char -> Parser ()
skipMany char = void $ zeroOrMore (satisfies (char==))

spaces :: Parser ()
spaces = skipMany ' '
