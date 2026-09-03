{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeResolver where

import AstTypes
  ( Callsite (Callsite, callsiteArgs, callsiteFunName),
    Class (Class, classMethods, classModules, className, classSuper),
    Expr (ECall, ELiteral, ENew),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    FunctionAnnotation (FunctionAnnotation),
    FunctionArg (FunctionArg, argDefaultValue, argName, argType),
    FunctionName (FunctionName),
    Literal (LitBool, LitInt, LitString),
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
getPlainDefs (ExprStmt _) = []

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

baseTypes :: TypeRefsMap
baseTypes =
  [("Bool", TBool), ("String", TString)]
    ++ [(pre ++ "Int" ++ len, TInt) | pre <- ["", "U"], len <- ["8", "16", "32", "64", "128"]]
    ++ [("Reference", TClass referenceClass), ("Object", TClass objectClass)]

resolveAst :: UnresolvedAst -> ResolvedAst
resolveAst uast = map (resolveStmt typeRefs) uast
  where
    typeRefs = baseTypes ++ concatMap getPlainDefs uast

resolveStmt :: TypeRefsMap -> UnresolvedStmt -> ResolvedStmt
resolveStmt trm (ClassStmt c) = ClassStmt $ resolveClass trm c
resolveStmt trm (ModuleStmt m) = ModuleStmt $ resolveModule trm m
resolveStmt trm (FunctionStmt f) = FunctionStmt $ resolveFunction trm f
resolveStmt trm (ExprStmt e) = ExprStmt $ resolveExpr trm e

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

resolveExpr :: TypeRefsMap -> Expr () -> Expr FixType
resolveExpr _ (ELiteral l) = ELiteral l
resolveExpr trm (ECall c) = ECall (resolveCallsite trm c)
resolveExpr trm (ENew tr) = ENew (resolveTypeRef trm tr)

resolveCallsite :: TypeRefsMap -> Callsite () -> Callsite FixType
resolveCallsite trm (Callsite {callsiteFunName, callsiteArgs}) =
  Callsite
    { callsiteFunName = callsiteFunName,
      callsiteArgs = map (resolveExpr trm) callsiteArgs
    }

mapType :: TypeRefsMap -> Type () -> FixType
mapType _ TInt = Fix TInt
mapType _ TBool = Fix TBool
mapType _ TString = Fix TString
mapType trm (TClass c) =
  Fix $
    if isObject c
      then
        TClass $
          Class
            { className = className c,
              classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = Fix TString},
              classModules = map (resolveTypeRef trm) (classModules c),
              classMethods = map (resolveFunction trm) (classMethods c)
            }
      else TClass $ resolveClass trm c
mapType trm (TFunction f) = Fix $ TFunction $ resolveFunction trm f
mapType trm (TModule m) = Fix $ TModule $ resolveModule trm m

isObject :: Class t -> Bool
isObject c = className c == TIdentifier "Object"

-- Lista de definiciones
-- 0 no existe
-- 1 no ambiguo
-- 2 es ambiguo
--
-- Falta modelar el Juicio que nos dice si/no y por qué callsite cumple restricciones
r :: ResolvedAst -> Callsite FixType -> [Function FixType]
r ast (Callsite {callsiteFunName, callsiteArgs}) =
  let s1 =
        foldr
          ( \stmt rec ->
              case stmt of
                (FunctionStmt f) ->
                  if funName f == callsiteFunName
                    && argsMatch callsiteArgs (funArgs f)
                    then f : rec
                    else rec
                _ -> rec
          )
          []
          ast
   in s1

fromFix :: FixType -> Type (Fix Type)
fromFix (Fix t) = t

argsMatch :: [Expr FixType] -> [FunctionArg FixType] -> Bool
argsMatch exprs fargs =
  length exprs == length fargs && and (zipWith simpleMatch exprs fargs)
  where
    simpleMatch :: Expr FixType -> FunctionArg FixType -> Bool
    simpleMatch _ (FunctionArg {argType = Nothing}) = True
    simpleMatch expr (FunctionArg {argType = Just tr}) =
      case (expr, fromFix $ tRefType tr) of
        (ELiteral (LitInt _), TInt) ->
          True
        (ELiteral (LitBool _), TBool) ->
          True
        (ELiteral (LitString _), TString) ->
          True
        (ENew (TypeRef {tRefType = Fix (TClass c1)}), TClass c2) ->
          c1 == c2 || c1 `isSubclassOf` c2
        (ECall _, _) ->
          error "Won't be using calls as expressions yet"
        _ ->
          False

superclass :: Class FixType -> Class FixType
superclass c = case tRefType $ classSuper c of
  Fix (TClass sc) -> sc
  _ -> if isObject c then c else error "Superclass of every class should be a TClass"

isSubclassOf :: Class FixType -> Class FixType -> Bool
isSubclassOf c1 c2 =
  let sc = superclass c1
   in sc == c2 || sc `isSubclassOf` c2
