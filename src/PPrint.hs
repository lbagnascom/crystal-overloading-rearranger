{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import AST.Nodes
  ( AST,
    Callsite (Callsite, callsiteArgs, callsiteFunName),
    Class (Class, classMethods, classModules, className, classSuper),
    Expr (ECall, ELiteral, ENew),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    FunctionArg (FunctionArg, argDefaultValue, argName, argTypeRestriction),
    FunctionName (FunctionName),
    Literal (LitBool, LitInt, LitString),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, ExprStmt, FunctionStmt, ModuleStmt),
  )
import AST.TypeIdentifier (TIdentifier (TIdentifier))
import AST.TypeReference (TypeRef (TypeRef, tRefName))
import AST.TypeRestriction (TypeRestriction (TResType, TResUnderscore))
import Data.List (intercalate)

printProgram :: AST t -> String
printProgram stmts = intercalate "\n" $ intercalate [""] $ map (printStmt 0) stmts

printStmt :: Int -> Stmt t -> [String]
printStmt n (ClassStmt c) = printClass n c
printStmt n (ModuleStmt m) = printModule n m
printStmt n (FunctionStmt def) = printFunction n def
printStmt n (ExprStmt expr) = [printExpr n expr]

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
    : map (\m -> indentation ++ "  include " ++ printIdentifier (tRefName m)) modules
    ++ intercalate [""] (map (printFunction (n + 2)) defs)
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '

printFunction :: Int -> Function t -> [String]
printFunction n (Function {funName = name, funArgs = args, funFreeVars = freeVars, funBody = body}) =
  (indentation ++ "def " ++ printFunctionName name ++ "(" ++ intercalate ", " (map printFunctionArg args) ++ ")" ++ maybeFreeVar)
    : map (("  " ++ indentation) ++) body
    ++ [indentation ++ "end"]
  where
    indentation = replicate n ' '
    maybeFreeVar = maybe "" (\xs -> " forall " ++ intercalate ", " xs) freeVars

printFunctionName :: FunctionName -> String
printFunctionName (FunctionName s) = s

printFunctionArg :: FunctionArg t -> String
printFunctionArg (FunctionArg {argName = name, argTypeRestriction = ty, argDefaultValue = defaultValue}) =
  name ++ maybeType ++ maybeDefaultValue
  where
    maybeType = maybe "" (\tyRestriction -> " : " ++ printTypeRestriction tyRestriction) ty
    maybeDefaultValue = maybe "" ((" = " ++) . printLiteral) defaultValue

printTypeRestriction :: TypeRestriction t -> String
printTypeRestriction TResUnderscore = "_"
printTypeRestriction (TResType tRef) = printIdentifier (tRefName tRef)

printLiteral :: Literal -> String
printLiteral (LitString s) = show s
printLiteral (LitBool True) = "true"
printLiteral (LitBool False) = "false"
printLiteral (LitInt n) = show n

printExpr :: Int -> Expr t -> String
printExpr n (ELiteral l) = replicate n ' ' ++ printLiteral l
printExpr n (ECall c) = replicate n ' ' ++ printCallsite c
printExpr n (ENew (TypeRef {tRefName})) = replicate n ' ' ++ printIdentifier tRefName ++ ".new"

printCallsite :: Callsite t -> String
printCallsite (Callsite {callsiteFunName, callsiteArgs}) =
  printFunctionName callsiteFunName
    ++ "("
    ++ concatMap (printExpr 0) callsiteArgs
    ++ ")"
