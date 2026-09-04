module AST.TypeRestriction where

import AST.TypeReference (TypeRef)

data TypeRestriction t
  = TResUnderscore
  | TResType (TypeRef t)
  deriving (Show, Eq)
