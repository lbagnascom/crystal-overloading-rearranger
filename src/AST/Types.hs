module AST.Types where

import AST.Fix (Fix)
import AST.Nodes
  ( Class,
    Function,
    Module,
    Stmt,
  )

data Type t
  = TInt
  | TBool
  | TString
  | TClass (Class t)
  | TFunction (Function t)
  | TModule (Module t)
  deriving (Show, Eq)

-- Unresolved

type UnresolvedType = ()

type UnresolvedStmt = Stmt UnresolvedType

type UnresolvedAst = [UnresolvedStmt]

-- Resolved

type FixType = Fix Type

type ResolvedStmt = Stmt FixType

type ResolvedAst = [ResolvedStmt]
