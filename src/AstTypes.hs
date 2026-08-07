module AstTypes where

type AST t = [Stmt t]

data TypeRef t = TypeRef
  { tRefName :: String,
    tRefType :: t
  }
  deriving (Show, Eq)

data Stmt t
  = ClassStmt (Class t)
  | ModuleStmt (Module t)
  | FunctionStmt (Function t)
  | UndiscoveredStmt String
  deriving (Show, Eq)

data Module t = Module
  { moduleName :: String,
    moduleMethods :: [Function t]
  }
  deriving (Show, Eq)

data Class t = Class
  { className :: String,
    classSuper :: TypeRef t,
    classModules :: [TypeRef t],
    classMethods :: [Function t]
  }
  deriving (Show, Eq)

data Function t = Function
  { funName :: String,
    funArgs :: [FunctionArg t],
    funFreeVars :: Maybe [String],
    funBody :: [String],
    funAnnotation :: Maybe String
  }
  deriving (Show, Eq)

data FunctionArg t = FunctionArg
  { argName :: String,
    argTypeName :: Maybe (TypeRef t),
    argDefaultValue :: Maybe Literal
  }
  deriving (Show, Eq)

data Literal
  = LitBool Bool
  | LitInt Int
  | LitString String
  deriving (Show, Eq)
