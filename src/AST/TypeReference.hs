module AST.TypeReference where

import AST.TypeIdentifier (TIdentifier)

data TypeRef t = TypeRef
  { tRefName :: TIdentifier,
    tRefType :: t
  }
  deriving (Show, Eq)
