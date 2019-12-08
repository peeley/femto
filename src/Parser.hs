module Parser where

import ParserType
import Lexer
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

data LispVal = String String | Integer Int | Float Float |
               Word String | Boolean Bool | List [LispVal]
               deriving (Eq, Ord)

instance Show LispVal where
    show (String s) = "\"" ++ s ++ "\""
    show (Integer i) = show i
    show (Word w) = w
    show (Boolean b) = if b then "#t" else "#f"
    show (List l) = "(" ++ (init . tail . show) l ++ ")"

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- zeroOrMore (letter <|> symbol <|> digit)
    char '"'
    return $ String str

parseInt :: Parser LispVal
parseInt = do
    neg <- optional '-'
    num <- oneOrMore digit
    return $ (Integer . read) (neg ++ num)

parseWord :: Parser LispVal
parseWord = do
    first <- symbol <|> letter
    rest <- zeroOrMore (letter <|> symbol <|> digit)
    return $ Word (first:rest)

parseBoolean :: Parser LispVal
parseBoolean = do
    char '#'
    val <- char 't' <|> char 'f'
    case val of
        't' -> return $ Boolean True
        'f' -> return $ Boolean False

parseList :: Parser LispVal
parseList = do
    char '('
    whitespace
    vals <- zeroOrMore parseExpr
    whitespace
    char ')'
    return $ List vals

parseQuote :: Parser LispVal
parseQuote = do
    char '\''
    expr <- parseExpr
    return $ List [Word "quote", expr]

parseExpr :: Parser LispVal
parseExpr = do
    whitespace
    parseList <|> parseQuote <|> parseBoolean  <|> parseString <|> 
        parseInt <|> parseWord

parseProgram :: Parser LispVal
parseProgram = do
    whitespace
    expr <- parseExpr
    whitespace
    return expr

parse :: String -> LispVal
parse program = case result of 
    Just (ast, "") -> ast
    Just (_, rest) -> error "Parse ended before end of file."
    Nothing -> error "Parse error."
    where result = runParser parseProgram program
