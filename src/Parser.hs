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
    parseList <|> parseQuote <|> parseBoolean  <|> parseString <|> 
        parseInt <|> parseWord

parseComment :: Parser ()
parseComment = do
    whitespace
    char ';'
    zeroOrMore $ satisfies ('\n' /=)
    char '\n'
    whitespace
    return ()


parseProgram :: Parser [LispVal]
parseProgram = do
    exprs <- zeroOrMore parseExpr
    whitespace
    zeroOrMore parseComment
    return exprs

parse :: String -> [LispVal]
parse program = case result of 
    Just (ast, "") -> ast
    Just (_, rest) -> error $ 
        "Parse ended with part of file remainging: " ++ rest
    Nothing -> error $ "Parse error while parsing: " ++ program
    where result = runParser parseProgram program
