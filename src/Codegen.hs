module Codegen where

import LispTypes
import Parser
import LLVM.AST

initModule :: AST.Module
initModule = emptyModule "femto compiler"

logic :: LLVM ()
logic = define double "main" [] $ \ptrToMain -> do
    let a = cons $ C.Float (F.Double 10)
    ret a


-- TODO: rest of the compiler
