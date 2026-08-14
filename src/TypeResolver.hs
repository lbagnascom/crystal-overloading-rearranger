module TypeResolver where

import AstTypes
  ( Class (Class, classMethods, classModules, className, classSuper),
    Function (Function, funArgs, funBody, funFreeVars, funName),
    FunctionArg (FunctionArg, argName, argType),
    FunctionName (FunctionName),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, FunctionStmt, ModuleStmt),
    TIdentifier (TIdentifier),
    TypeRef (TypeRef, tRefName, tRefType),
  )

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

-- data TypeRestriction
-- = TRUnderscore
-- \| TRType Type

data FixType = Type FixType

type ResolvedStmt = Stmt FixType

type ResolvedAst = [ResolvedStmt]

-- Type Resolution

-- TODO: use TIdentifier
type TypeRefsMap = [(String, Type ())]

fromIdentifier :: TIdentifier -> String
fromIdentifier (TIdentifier s) = s

fromFnName :: FunctionName -> String
fromFnName (FunctionName s) = s

getPlainDefs :: UnresolvedStmt -> [(String, Type ())]
getPlainDefs (ClassStmt c) = [(fromIdentifier $ className c, TClass c)]
getPlainDefs (ModuleStmt m) = [(fromIdentifier $ moduleName m, TModule m)]
getPlainDefs (FunctionStmt f) = [(fromFnName $ funName f, TFunction f)]

-- resolveTypes :: UnresolvedAst -> ResolvedAst
-- resolveTypes uast = map resolveStmt uast
--   where
--     typeRefs = concatMap getPlainDefs uast

-- resolveStmt :: TypeRefsMap -> UnresolvedStmt -> ResolvedStmt
-- resolveStmt trm (ClassStmt c) =
--   ClassStmt
--     ( Class
--         { className = className c,
--           classSuper = TypeRef {tRefName = className c, tRefType = TClass},
--           classModules = map resolveModule $ (\tr -> _),
--           classMethods = map resolveFunction $ (\tr -> _)
--         }
--     )
-- resolveStmt trm (ModuleStmt m) = ModuleStmt _
-- resolveStmt trm (FunctionStmt f) = FunctionStmt _
--
-- resolveModule :: TypeRefsMap -> Module () -> Module Type
-- resolveModule = _
--
resolveFunction :: TypeRefsMap -> Function () -> Function FixType
resolveFunction trm f = f {funArgs = map (resolveArgs trm) (funArgs f)}

resolveArgs :: TypeRefsMap -> FunctionArg () -> FunctionArg FixType
resolveArgs trm fArg =
  let fArgName = argName fArg

      cType :: Maybe (TypeRef (Type ()))
      cType = do
        fArgType <- argType fArg
        let refName = (tRefName fArgType)
        refType <- lookup (fromIdentifier refName) trm
        Just $ TypeRef {tRefName = refName, tRefType = refType}

      nType :: Maybe (TypeRef FixType)
      nType = fmap (\t -> t {tRefType = mapType (tRefType t)}) cType
   in fArg {argType = nType}

mapType :: Type () -> FixType
mapType (TInt) = Type _
mapType (TBool) = _
mapType (TString) = _
mapType (TClass c) = _
mapType (TFunction f) = _
mapType (TModule m) = _
