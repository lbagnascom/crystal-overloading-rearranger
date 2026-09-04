module AST.Types where

import AST.Nodes
  ( Class,
    Function,
    Module,
    Stmt,
  )
import TypeResolution.Fix (Fix)

data Type t
  = TInt
  | TBool
  | TString
  | TClass (Class t)
  | TFunction (Function t)
  | TModule (Module t)
  deriving (Show, Eq)

data TypeRestriction t
  = TRUnderscore
  | TRType (Type t)

type FixType = Fix Type

-- Unresolved

type UnresolvedType = ()

type UnresolvedStmt = Stmt UnresolvedType

type UnresolvedAst = [UnresolvedStmt]

-- Resolved

type ResolvedStmt = Stmt FixType

type ResolvedAst = [ResolvedStmt]
