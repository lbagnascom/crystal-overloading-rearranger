{-# LANGUAGE NamedFieldPuns #-}

module TypeResolution.Resolver where

import AST.Nodes
  ( Callsite (Callsite, callsiteArgs, callsiteFunName),
    Class (Class, classMethods, classModules, className, classSuper),
    Expr (ECall, ELiteral, ENew),
    Function (funArgs, funName),
    FunctionArg (FunctionArg, argDefaultValue, argName, argType),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, ExprStmt, FunctionStmt, ModuleStmt),
    TIdentifier (TIdentifier),
    TypeRef (TypeRef, tRefName, tRefType),
    fromFnName,
    fromIdentifier,
  )
import AST.Types
  ( FixType,
    ResolvedAst,
    ResolvedStmt,
    Type (TBool, TClass, TFunction, TInt, TModule, TString),
    UnresolvedAst,
    UnresolvedStmt,
  )
import Data.Maybe (fromJust)
import TypeResolution.Fix (Fix (Fix))

-- Type Resolution

type TypeRefsMap = [(String, Type ())]

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
      let refName = tRefName justArgType
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
    if className c == TIdentifier "Object"
      then
        let obj =
              TClass $
                Class
                  { className = className c,
                    classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = Fix obj},
                    classModules = map (resolveTypeRef trm) (classModules c),
                    classMethods = map (resolveFunction trm) (classMethods c)
                  }
         in obj
      else TClass $ resolveClass trm c
mapType trm (TFunction f) = Fix $ TFunction $ resolveFunction trm f
mapType trm (TModule m) = Fix $ TModule $ resolveModule trm m
