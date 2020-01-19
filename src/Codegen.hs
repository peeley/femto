{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import LispTypes
import Parser
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Module
import LLVM.Context
import qualified Data.ByteString as BS
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

int :: Type
int = IntegerType 32

defAdd :: AST.Definition
defAdd = GlobalDefinition functionDefaults {
    name = "add",
    parameters = (
        [ Parameter int (Name "a") [],
          Parameter int (Name "b") [] ]
        , False ),
    returnType = int,
    basicBlocks = [body] }
    where
        body = BasicBlock (Name "entry")
                [ Name "result" :=
                    Add 
                        False -- no signed wrap
                        False -- no unsigned wrap
                        (LocalReference int (Name "a"))
                        (LocalReference int (Name "b"))
                        []]
                (Do $ Ret (Just (LocalReference int (Name "result"))) [])


initModule :: AST.Module
initModule = defaultModule {
    AST.moduleName = "test module",
    AST.moduleDefinitions = [defAdd]
    }

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
    llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
    BS.putStrLn llvm

