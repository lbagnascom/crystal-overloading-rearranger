{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import AstTypes
  ( AST,
    Class (Class, classMethods, classModules, className, classSuper),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    FunctionArg (FunctionArg, argDefaultValue, argName, argType),
    FunctionName (FunctionName),
    Literal (LitBool, LitInt, LitString),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, FunctionStmt, ModuleStmt),
    TIdentifier (TIdentifier),
    TypeRef (tRefName),
  )
import Data.List (intercalate)

printProgram :: AST t -> String
printProgram stmts = intercalate "\n" $ intercalate [""] $ map (printStmt 0) stmts

printStmt :: Int -> Stmt t -> [String]
printStmt n (ClassStmt c) = printClass n c
printStmt n (ModuleStmt m) = printModule n m
printStmt n (FunctionStmt def) = printFunction n def

printIdentifier :: TIdentifier -> String
printIdentifier (TIdentifier s) = s

printModule :: Int -> Module t -> [String]
printModule n (Module {moduleName = name, moduleMethods = defs}) =
  (indentation ++ "module " ++ printIdentifier name)
    : intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printClass :: Int -> Class t -> [String]
printClass n (Class {className = name, classSuper = super, classMethods = defs, classModules = modules}) =
  (indentation ++ "class " ++ printIdentifier name ++ " < " ++ printIdentifier (tRefName super))
    : (map (\m -> indentation ++ "  include " ++ printIdentifier (tRefName m)) modules)
    ++ intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printFunction :: Int -> Function t -> [String]
printFunction n (Function {funName = name, funArgs = args, funFreeVars = freeVars, funBody = body}) =
  (indentation ++ "def " ++ printFName name ++ "(" ++ intercalate ", " (map printFunctionArg args) ++ ")" ++ maybeFreeVar)
    : map (("  " ++ indentation) ++) body
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '
    maybeFreeVar = maybe "" (\xs -> " forall " ++ intercalate ", " xs) freeVars
    printFName (FunctionName s) = s

printFunctionArg :: FunctionArg t -> String
printFunctionArg (FunctionArg {argName = name, argType = ty, argDefaultValue = defaultValue}) =
  name ++ maybeType ++ maybeDefaultValue
  where
    maybeType = maybe "" (\typeRef -> " : " ++ printIdentifier (tRefName typeRef)) ty
    maybeDefaultValue = maybe "" ((" = " ++) . printLiteral) defaultValue

printLiteral :: Literal -> String
printLiteral (LitString s) = show s
printLiteral (LitBool True) = "true"
printLiteral (LitBool False) = "false"
printLiteral (LitInt n) = show n

printUndiscovered :: Int -> String -> [String]
printUndiscovered n s = [replicate n ' ' ++ s]
