{-# LANGUAGE OverloadedRecordDot #-}

module AST.Nodes where

import AST.TypeIdentifier (TIdentifier (TIdentifier), fromIdentifier)
import AST.TypeReference (TypeRef)
import AST.TypeRestriction (TypeRestriction)

type AST t = [Stmt t]

-- Statements

data Stmt t
  = ClassStmt (Class t)
  | ModuleStmt (Module t)
  | FunctionStmt (Function t)
  | ExprStmt (Expr t)
  deriving (Show, Eq)

data Module t = Module
  { moduleName :: TIdentifier,
    moduleMethods :: [Function t]
  }
  deriving (Show, Eq)

data Class t = Class
  { className :: TIdentifier,
    classSuper :: TypeRef t,
    classModules :: [TypeRef t],
    classMethods :: [Function t]
  }

instance (Eq t) => Eq (Class t) where
  c1 == c2 =
    (c1.className == TIdentifier "Object") == (c2.className == TIdentifier "Object")
      || ( c1.className == c2.className
             && c1.classModules == c2.classModules
             && c1.classSuper == c2.classSuper
             && c1.classMethods == c2.classMethods
         )

instance Show (Class t) where
  show c = fromIdentifier c.className

newtype FunctionName = FunctionName String
  deriving (Show, Eq)

fromFnName :: FunctionName -> String
fromFnName (FunctionName s) = s

newtype FunctionAnnotation = FunctionAnnotation String
  deriving (Show, Eq)

data Function t = Function
  { funName :: FunctionName,
    funArgs :: [FunctionArg t],
    funFreeVars :: Maybe [String],
    funBody :: [String],
    funAnnotation :: Maybe FunctionAnnotation
  }
  deriving (Show, Eq)

data FunctionArg t = FunctionArg
  { argName :: String,
    argTypeRestriction :: Maybe (TypeRestriction t),
    argDefaultValue :: Maybe Literal
  }
  deriving (Show, Eq)

-- Expressions

data Expr t
  = ELiteral Literal
  | ECall (Callsite t)
  | ENew (TypeRef t)
  deriving (Show, Eq)

data Callsite t = Callsite
  { callsiteFunName :: FunctionName,
    callsiteArgs :: [Expr t]
  }
  deriving (Show, Eq)

data Literal
  = LitBool Bool
  | LitInt Int
  | LitString String
  deriving (Show, Eq)
