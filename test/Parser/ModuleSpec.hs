{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.ModuleSpec where

import AstTypes (Class (..), Function (..), FunctionArg (..), FunctionName (..), Literal (..), Module (..), Stmt (..), TIdentifier (..), TypeRef (..))
import Data.Either (fromRight)
import Data.String.Interpolate (__i)
import Parser
  ( parseBool,
    parseClass,
    parseFunction,
    parseFunctionArg,
    parseInteger,
    parseProgram,
    parseString,
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolver (Fix (..), FixType, ResolvedAst, Type (..), UnresolvedAst, resolveAst)

spec :: Spec
spec = do
  describe "parseLiterals" $ do
    it "parses a literal integer" $
      parse parseInteger "" "1435" `shouldParse` (LitInt 1435)
