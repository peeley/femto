module Parser where

import ParserType
import Lexer
import Control.Applicative ((<|>))

data LispVal = String String | Integer Int | Float Float |
               Word String | Boolean Bool | List [LispVal]
               deriving Show

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
    spaces
    vals <- zeroOrMore parseExpr
    spaces
    char ')'
    return $ List vals

parseExpr :: Parser LispVal
parseExpr = 
    parseList <|> parseBoolean  <|> parseString <|> parseInt <|> parseWord
