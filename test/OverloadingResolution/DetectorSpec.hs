{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OverloadingResolution.DetectorSpec where

import AstTypes
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
  ( FixType,
    ResolvedAst,
    Type (..),
    UnresolvedAst,
    baseTypes,
    mapType,
    objectClass,
    referenceClass,
    resolveAst,
  )

simpleFunctionArg :: FunctionArg FixType
simpleFunctionArg =
  FunctionArg
    { argName = "x",
      argType = Nothing,
      argDefaultValue = Just $ LitBool True
    }

spec :: Spec
spec = do
  it "No args match" $
    argsMatch [] [] `shouldBe` True
  it "No args match with optional value" $
    argsMatch
      []
      [simpleFunctionArg]
      `shouldBe` True
  it "One arg matches using default value" $
    argsMatch
      [ELiteral $ LitBool False]
      [simpleFunctionArg {argDefaultValue = Just $ LitBool True}]
      `shouldBe` True
  it "One arg matches using default value and type restriction" $
    argsMatch
      [ELiteral $ LitBool False]
      [ simpleFunctionArg
          { argType = Just $ TypeRef {tRefName = TIdentifier "Bool", tRefType = Fix TBool},
            argDefaultValue = Just $ LitBool True
          }
      ]
      `shouldBe` True
