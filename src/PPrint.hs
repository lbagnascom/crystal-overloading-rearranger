{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import AstTypes
import Data.List (intercalate)
printProgram :: (Show t) => AST t -> String
printProgram stmts = intercalate "\n" $ intercalate [""] $ map (printStmt 0) stmts

printStmt :: (Show t) => Int -> Stmt t -> [String]
printStmt n (ClassStmt c) = printClass n c
printStmt n (ModuleStmt m) = printModule n m
printStmt n (FunctionStmt def) = printFunction n def
printStmt n (UndiscoveredStmt s) = printUndiscovered n s

printModule :: (Show t) => Int -> Module t -> [String]
printModule n (Module {moduleName = name, moduleMethods = defs}) =
  (indentation ++ "module " ++ name)
    : intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printClass :: (Show t) => Int -> Class t -> [String]
printClass n (Class {className = name, classSuper = super, classMethods = defs, classModules = modules}) =
  (indentation ++ "class " ++ name ++ " < " ++ super)
    : (map (\m -> indentation ++ "  include " ++ m) modules)
    ++ intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printFunction :: (Show t) => Int -> Function t -> [String]
printFunction n (Function {funName = name, funArgs = args, funFreeVars = freeVars, funBody = body}) =
  (indentation ++ "def " ++ name ++ "(" ++ intercalate ", " (map printFunctionArg args) ++ ")" ++ maybeFreeVar)
    : map (("  " ++ indentation) ++) body
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '
    maybeFreeVar = maybe "" (\xs -> " forall " ++ intercalate ", " xs) freeVars

printFunctionArg :: (Show t) => FunctionArg t -> String
printFunctionArg (FunctionArg {argName = name, argTypeName = ty, argDefaultValue = defaultValue}) =
  name ++ maybeType ++ maybeDefaultValue
  where
    maybeType = maybe "" (\typeName -> " : " ++ show typeName) ty
    maybeDefaultValue = maybe "" ((" = " ++) . printLiteral) defaultValue

printLiteral :: Literal -> String
printLiteral (CrString s) = show s
printLiteral (CrBool True) = "true"
printLiteral (CrBool False) = "false"
printLiteral (CrInt n) = show n

printUndiscovered :: Int -> String -> [String]
printUndiscovered n s = [replicate n ' ' ++ s]
