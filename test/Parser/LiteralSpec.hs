{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.LiteralSpec where

import AstTypes
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
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import Data.Either (fromRight)
import TypeResolver (resolveTypes, ResolvedAst, UnresolvedAst, Type(..), Fix(..), FixType)

main :: IO ()
main = hspec $ do
  describe "parseLiterals" $ do
    it "parses a literal integer" $
      parse parseInteger "" "1435" `shouldParse` (LitInt 1435)

    it "fails on invalid integer" $
      parse parseInteger "" `shouldFailOn` "abcb123"

    it "parses a literal string" $
      parse parseString "" "\"abcdef\"" `shouldParse` LitString "abcdef"

    it "fails parsing a literal string non closing" $
      parse parseString "" `shouldFailOn` "\"abcb"

    it "parses true boolean" $
      parse parseBool "" "true" `shouldParse` LitBool True

    it "parses false boolean" $
      parse parseBool "" "false" `shouldParse` LitBool False
