module TypeResolver where

import AstTypes
  ( Class (Class, classMethods, classModules, className, classSuper),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, FunctionStmt, ModuleStmt),
    TypeRef (TypeRef, tRefName, tRefType),
  )

-- Unresolved

type UnresolvedType = ()

type UnresolvedStmt = Stmt UnresolvedType

type UnresolvedAst = [UnresolvedStmt]

-- Resolved

-- Separar restriccion de tipos
-- new alloc
-- sacar undiscoveredStmt

data Type
  = TInt
  | TBool
  | TString
  | TClass
  | TFunction
  | TModule
  deriving (Show, Eq)

data Restriction
  = RUnderscore
  | RType Type

type ResolvedStmt = Stmt Type

type ResolvedAst = [ResolvedStmt]

-- Type Resolution

type TypeRefsMap = [(String, Type)]

getPlainDefs :: UnresolvedStmt -> [(String, Type)]
getPlainDefs (ClassStmt c) = [(className c, TClass)]
getPlainDefs (ModuleStmt m) = [(moduleName m, TModule)]
getPlainDefs (FunctionStmt f) = [(funName f, TFunction)]

resolveTypes :: UnresolvedAst -> ResolvedAst
resolveTypes uast = map resolveStmt uast
  where
    typeRefs = concatMap getPlainDefs uast

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

resolveModule :: TypeRefsMap -> Module () -> Module Type
resolveModule = _

resolveFunction :: TypeRefsMap -> Function () -> Function Type
resolveFunction = _
