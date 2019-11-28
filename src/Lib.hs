module Lib ( 
parse
) where

import ParserType
import Parser
import Lexer

parse :: String -> Maybe (LispVal, String)
parse = runParser parseExpr
