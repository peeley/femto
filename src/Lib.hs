module Lib ( 
parse
) where

import ParserType
import Lexer

data LispVal = List [LispVal] | Atom String |
               String String | Number Float |
               Bool Bool deriving Show


parse :: String -> LispVal
parse = undefined
