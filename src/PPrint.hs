{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Parser

printProgram :: CrystalProgram -> String
printProgram stmt = intercalate "\n" $ concatMap (printStmt 0) stmt

printStmt :: Int -> Stmt -> [String]
printStmt n (ClassStmt c) = printClass n c
printStmt n (FunctionStmt def) = printFunction n def
printStmt n (UndiscoveredStmt s) = printUndiscovered n s

printClass :: Int -> Class -> [String]
printClass n (Class {className = name, superClass = super, methods = defs}) =
  (indentation ++ "class " ++ name ++ " < " ++ super)
    : concatMap (printFunction (n + 2)) defs
    ++ [indentation ++ "end\n"]
  where
    indentation = replicate n ' '

printFunction :: Int -> Function -> [String]
printFunction n (Function {funName = name, funArgs = args, funFreeVar = freeVar, funBody = body}) =
  (indentation ++ "def " ++ name ++ "(" ++ (intercalate ", " $ map printFunctionArg args) ++ ")" ++ maybeFreeVar)
    : map (("  " ++ indentation) ++) body
    ++ [indentation ++ "end\n"]
  where
    indentation = replicate n ' '
    maybeFreeVar = (fromMaybe "" ((" forall " ++) <$> freeVar))

printFunctionArg :: FunctionArg -> String
printFunctionArg (FunctionArg {argName = name, argType = ty, argDefaultValue = defaultValue}) =
  name ++ maybeType ++ maybeDefaultValue
  where
    maybeType = fromMaybe "" ((" : " ++) <$> ty)
    maybeDefaultValue = fromMaybe "" ((" = " ++) . printLiteral <$> defaultValue)

printLiteral :: Literal -> String
printLiteral (CrString s) = show s
printLiteral (CrBool True) = "true"
printLiteral (CrBool False) = "false"
printLiteral (CrInt n) = show n

printUndiscovered :: Int -> String -> [String]
printUndiscovered n s = [replicate n ' ' ++ s]
