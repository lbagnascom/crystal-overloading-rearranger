module AST.TypeIdentifier where

fromIdentifier :: TIdentifier -> String
fromIdentifier (TIdentifier s) = s

newtype TIdentifier = TIdentifier String
  deriving (Show, Eq)
