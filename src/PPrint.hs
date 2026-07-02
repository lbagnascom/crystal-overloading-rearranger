{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import Data.List (intercalate)
import Parser

printProgram :: CrystalProgram -> String
printProgram stmts = intercalate "\n" $ intercalate [""] $ map (printStmt 0) stmts

printStmt :: Int -> Stmt -> [String]
printStmt n (ClassStmt c) = printClass n c
printStmt n (ModuleStmt m) = printModule n m
printStmt n (FunctionStmt def) = printFunction n def
printStmt n (UndiscoveredStmt s) = printUndiscovered n s

printModule :: Int -> Module -> [String]
printModule n (Module {moduleName = name, moduleMethods = defs}) =
  (indentation ++ "module " ++ name)
    : intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printClass :: Int -> Class -> [String]
printClass n (Class {className = name, classSuper = super, classMethods = defs, classModules = modules}) =
  (indentation ++ "class " ++ name ++ " < " ++ super)
    :  (map (\m -> indentation ++ "  include " ++ m) modules)
    ++ intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printFunction :: Int -> Function -> [String]
printFunction n (Function {funName = name, funArgs = args, funFreeVars = freeVars, funBody = body}) =
  (indentation ++ "def " ++ name ++ "(" ++ intercalate ", " (map printFunctionArg args) ++ ")" ++ maybeFreeVar)
    : map (("  " ++ indentation) ++) body
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '
    maybeFreeVar = maybe "" (\xs -> " forall " ++ intercalate ", " xs) freeVars

printFunctionArg :: FunctionArg -> String
printFunctionArg (FunctionArg {argName = name, argType = ty, argDefaultValue = defaultValue}) =
  name ++ maybeType ++ maybeDefaultValue
  where
    maybeType = maybe "" (" : " ++) ty
    maybeDefaultValue = maybe "" ((" = " ++) . printLiteral) defaultValue

printLiteral :: Literal -> String
printLiteral (CrString s) = show s
printLiteral (CrBool True) = "true"
printLiteral (CrBool False) = "false"
printLiteral (CrInt n) = show n

printUndiscovered :: Int -> String -> [String]
printUndiscovered n s = [replicate n ' ' ++ s]
