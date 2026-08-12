module TypeResolver where

import AstTypes
  ( Class (..),
    Function (..),
    Module (..),
    Stmt (..),
    TypeRef (..),
  )

-- Unresolved

type UnresolvedType = ()

type UnresolvedStmt = Stmt UnresolvedType

type UnresolvedAst = [UnresolvedStmt]

-- Resolved
--
-- Separar restriccion de tipos
-- new alloc
-- sacar undiscoveredStmt
--

data SomeType t
  = TInt
  | TBool
  | TString
  | TUnderscore
  | TClass (Class t)
  | TFunction (Function t)
  | TModule (Module t)
  deriving (Show, Eq)

data Restriction
  = RUnderscore
  | RType Type

data Type = SomeType Type

type ResolvedStmt = Stmt Type

type ResolvedAst = [ResolvedStmt]

-- Type Resolution

type TypeRefsMap = [(String, Type)]

getPlainDefs :: UnresolvedStmt -> [(String, SomeType ())]
getPlainDefs (ClassStmt c) = [(className c, TClass)]
getPlainDefs (ModuleStmt m) = [(moduleName m, TModule)]
getPlainDefs (FunctionStmt f) = [(funName f, TFunction)]
getPlainDefs (UndiscoveredStmt _) = []

resolveTypes :: UnresolvedAst -> ResolvedAst
resolveTypes uast = map resolveStmt uast
  where
    typeRefs = concatMap getPlainDefs uast

--
resolveStmt :: TypeRefsMap -> UnresolvedStmt -> ResolvedStmt
resolveStmt trm (ClassStmt c) =
  ClassStmt
    ( Class
        { className = className c,
          classSuper = TypeRef {tRefName = className c, tRefType = TClass},
          classModules = map resolveModule $ (\tr -> _),
          classMethods = map resolveFunction $ (\tr -> _)
        }
    )
resolveStmt trm (ModuleStmt m) = ModuleStmt _
resolveStmt trm (FunctionStmt f) = FunctionStmt _
resolveStmt trm (UndiscoveredStmt s) = UndiscoveredStmt s

resolveModule :: TypeRefsMap -> Module () -> Module Type
resolveModule = _

resolveFunction :: TypeRefsMap -> Function () -> Function Type
resolveFunction = _
