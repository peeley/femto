module Eval where

import Parser

eval :: LispVal -> IO ()
eval (String str) = print str
eval (Integer int) = print int
eval (Word name) = print name
eval (Boolean bool) = print bool
eval (List [Word "quote", val]) = print val
eval (List (Word fun : val)) = undefined
eval _ = undefined

