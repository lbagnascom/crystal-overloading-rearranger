{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OverloadingResolution.DetectorSpec where

import AST.Nodes
  ( Callsite (..),
    Class (..),
    Expr (..),
    Function (..),
    FunctionArg (..),
    FunctionName (..),
    Literal (..),
    Module (..),
    Stmt (..),
    TIdentifier (..),
    TypeRef (..),
  )
import AST.Types (FixType, ResolvedAst, Type (TBool, TString), UnresolvedAst)
import Data.Either (fromRight)
import Data.String.Interpolate (__i)
import OverloadingResolution.Detector
  ( argsMatch,
    destroyClass,
    isSubclassOf,
    matchingCallsite,
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolution.Fix (Fix (..))
import TypeResolution.Resolver
  ( baseTypes,
    mapType,
    objectClass,
    referenceClass,
    resolveAst,
  )

stringFunctionArg :: FunctionArg FixType
stringFunctionArg =
  FunctionArg
    { argName = "s",
      argType = Just $ TypeRef {tRefName = TIdentifier "String", tRefType = Fix TString},
      argDefaultValue = Nothing
    }

booleanWithDefaultValue :: FunctionArg FixType
booleanWithDefaultValue =
  FunctionArg
    { argName = "x",
      argType = Nothing,
      argDefaultValue = Just $ LitBool True
    }

spec :: Spec
spec = do
  it "Zero args match with no function args" $
    argsMatch [] [] `shouldBe` True
  it "Default value and no restriction allows zero args" $
    argsMatch
      []
      [booleanWithDefaultValue]
      `shouldBe` True
  it "Default value and no restriction allows one arg" $
    argsMatch
      [ELiteral $ LitBool False]
      [booleanWithDefaultValue]
      `shouldBe` True
  it "Default value and type restriction allows one arg" $
    argsMatch
      [ELiteral $ LitBool False]
      [ booleanWithDefaultValue
          { argType =
              Just $ TypeRef {tRefName = TIdentifier "Bool", tRefType = Fix TBool}
          }
      ]
      `shouldBe` True
  it "Two args don't match if some value does not type" $
    argsMatch
      [ELiteral $ LitBool False, ELiteral $ LitBool True]
      [booleanWithDefaultValue, stringFunctionArg]
      `shouldBe` False
  it "Two args match when all values satisfy type restriction " $
    argsMatch
      [ELiteral $ LitBool False, ELiteral $ LitString "foo"]
      [booleanWithDefaultValue, stringFunctionArg]
      `shouldBe` True
