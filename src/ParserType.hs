module ParserType where

import Data.Maybe (isJust)
import Control.Applicative
import Data.Map.Strict as M
import Data.IORef

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- apply function to parsed AST to return new parser
instance Functor Parser where
    fmap f (Parser p) = Parser newParse
        where newParse input = case p input of
                            Just (val, str) -> Just(f val, str)
                            Nothing -> Nothing

-- applies wrapped function in p1 to output of p2
instance Applicative Parser where
    pure x = Parser $ \str -> Just (x, str)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, str1) <- p1 input
        (x, str2) <- p2 str1
        return (f x, str2)

-- apply function to parser itself to return new parser
instance Monad Parser where
    return = pure
    (Parser p1) >>= f = Parser $ \input -> do
        (x, str) <- p1 input
        runParser (f x) str

-- if first parser returns nothing, try the second parser
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> do
        let out = p1 input
        if isJust out then out
        else p2 input

