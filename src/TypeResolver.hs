module TypeResolver where

import AstTypes (Stmt)

type UnresolvedStmt = Stmt String

type UnresolvedAst = [UnresolvedStmt]

type ResolvedStmt = Stmt Type

type ResolvedAst = [ResolvedStmt]

data Type
  = TInt
  | TBool
  | TString
  | TClass
  | TModule
  deriving (Show, Eq)
