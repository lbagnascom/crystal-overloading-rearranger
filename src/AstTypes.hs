module AstTypes where

type AST t = [Stmt t]

-- Types

data TIdentifier = TIdentifier String
  deriving (Show, Eq)

data TypeRef t = TypeRef
  { tRefName :: TIdentifier,
    tRefType :: t
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data FunctionName = FunctionName String
  deriving (Show, Eq)

data FunctionAnnotation = FunctionAnnotation String
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
    argType :: Maybe (TypeRef t),
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
