{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeResolver where

import AstTypes
  ( Class (Class, classMethods, classModules, className, classSuper),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    FunctionAnnotation (FunctionAnnotation),
    FunctionArg (FunctionArg, argDefaultValue, argName, argType),
    FunctionName (FunctionName),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, ExprStmt, FunctionStmt, ModuleStmt),
    TIdentifier (TIdentifier),
    TypeRef (TypeRef, tRefName, tRefType),
  )
import Data.Maybe (fromJust)

-- Unresolved

type UnresolvedType = ()

type UnresolvedStmt = Stmt UnresolvedType

type UnresolvedAst = [UnresolvedStmt]

-- Resolved

data Type t
  = TInt
  | TBool
  | TString
  | TClass (Class t)
  | TFunction (Function t)
  | TModule (Module t)
  deriving (Show, Eq)

-- TODO: add TypeRestrictions to FunctionArg to support TRUnderscore
-- data TypeRestriction
-- = TRUnderscore
-- \| TRType Type

data Fix t = Fix (t (Fix t))

deriving instance (Show (t (Fix t))) => (Show (Fix t))

deriving instance (Eq (t (Fix t))) => (Eq (Fix t))

type FixType = Fix Type

type ResolvedStmt = Stmt FixType

type ResolvedAst = [ResolvedStmt]

-- Type Resolution

type TypeRefsMap = [(String, Type ())]

fromIdentifier :: TIdentifier -> String
fromIdentifier (TIdentifier s) = s

fromFnName :: FunctionName -> String
fromFnName (FunctionName s) = s

getPlainDefs :: UnresolvedStmt -> [(String, Type ())]
getPlainDefs (ClassStmt c) = [(fromIdentifier $ className c, TClass c)]
getPlainDefs (ModuleStmt m) = [(fromIdentifier $ moduleName m, TModule m)]
getPlainDefs (FunctionStmt f) = [(fromFnName $ funName f, TFunction f)]
getPlainDefs (ExprStmt e) = _

referenceClass :: Class ()
referenceClass =
  Class
    { className = TIdentifier "Reference",
      classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = ()},
      classModules = [],
      classMethods = []
    }

objectClass :: Class ()
objectClass =
  Class
    { className = TIdentifier "Object",
      classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = ()},
      classModules = [],
      classMethods = []
    }

resolveTypes :: UnresolvedAst -> ResolvedAst
resolveTypes uast = map (resolveStmt typeRefs) uast
  where
    baseTypes =
      [("Bool", TBool), ("String", TString)]
        ++ [(pre ++ "Int" ++ len, TInt) | pre <- ["", "U"], len <- ["8", "16", "32", "64", "128"]]
        ++ [("Reference", TClass referenceClass), ("Object", TClass objectClass)]
    typeRefs = baseTypes ++ concatMap getPlainDefs uast

resolveStmt :: TypeRefsMap -> UnresolvedStmt -> ResolvedStmt
resolveStmt trm (ClassStmt c) = ClassStmt $ resolveClass trm c
resolveStmt trm (ModuleStmt m) = ModuleStmt $ resolveModule trm m
resolveStmt trm (FunctionStmt f) = FunctionStmt $ resolveFunction trm f
resolveStmt trm (ExprStmt e) = _

resolveModule :: TypeRefsMap -> Module () -> Module FixType
resolveModule trm (Module {moduleName, moduleMethods}) =
  Module
    { moduleName = moduleName,
      moduleMethods = map (resolveFunction trm) moduleMethods
    }

resolveFunction :: TypeRefsMap -> Function () -> Function FixType
resolveFunction trm f = f {funArgs = map (resolveArgs trm) (funArgs f)}

resolveArgs :: TypeRefsMap -> FunctionArg () -> FunctionArg FixType
resolveArgs trm (FunctionArg {argName, argType, argDefaultValue}) =
  FunctionArg
    { argName = argName,
      argType = resolvedArgTypeRef,
      argDefaultValue = argDefaultValue
    }
  where
    resolvedArgTypeRef :: Maybe (TypeRef FixType)
    resolvedArgTypeRef = do
      justArgType <- argType
      let refName = (tRefName justArgType)
      refType <- lookup (fromIdentifier refName) trm
      Just $
        TypeRef
          { tRefName = refName,
            tRefType = mapType trm refType
          }

resolveTypeRef :: TypeRefsMap -> TypeRef () -> TypeRef FixType
resolveTypeRef trm (TypeRef {tRefName}) =
  let plainType :: Type ()
      plainType = fromJust $ lookup (fromIdentifier tRefName) trm
   in TypeRef
        { tRefName = tRefName,
          tRefType = mapType trm plainType
        }

resolveClass :: TypeRefsMap -> Class () -> Class FixType
resolveClass trm (Class {className, classSuper, classModules, classMethods}) =
  Class
    { className = className,
      classSuper = resolveTypeRef trm classSuper,
      classModules = map (resolveTypeRef trm) classModules,
      classMethods = map (resolveFunction trm) classMethods
    }

mapType :: TypeRefsMap -> Type () -> FixType
mapType _ TInt = Fix TInt
mapType _ TBool = Fix TBool
mapType _ TString = Fix TString
mapType trm (TClass c) =
  if className c == TIdentifier "Object" -- TODO: encontrar mejor forma de cortar con la herencia infinita
    then Fix TString
    else Fix $ TClass $ resolveClass trm c
mapType trm (TFunction f) = Fix $ TFunction $ resolveFunction trm f
mapType trm (TModule m) = Fix $ TModule $ resolveModule trm m

-- Lista de definiciones
-- 0 no existe
-- 1 no ambiguo
-- 2 es ambiguo
--
-- Falta modelar el Juicio que nos dice si/no y por qué callsite cumple restricciones
r :: ResolvedAst -> () -> [Function t]
r = undefined
