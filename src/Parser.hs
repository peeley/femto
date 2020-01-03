module Parser where

import ParserType
import LispTypes
import Lexer
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Control.Monad (void)

parseString :: Parser LispVal
parseString = do
    char '"' <|> char '\''
    str <- zeroOrMore $ satisfies ('"' /=)
    char '"' <|> char '\''
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
    zeroOrMore parseComment
    whitespace
    parseList <|> parseQuote <|> parseBoolean  <|> parseString <|> 
        parseInt <|> parseWord

parseComment :: Parser ()
parseComment = do
    char ';'
    zeroOrMore $ satisfies ('\n' /=)
    char '\n'
    return ()


parseProgram :: Parser LispVal
parseProgram = do
    zeroOrMore parseComment
    whitespace
    expr <- parseExpr
    whitespace
    zeroOrMore parseComment
    return expr

parse :: String -> LispVal
parse program = case result of 
    Just (ast, "") -> ast
    Just (_, rest) -> error "Parse ended before end of file."
    Nothing -> error "Parse error."
    where result = runParser parseProgram program
