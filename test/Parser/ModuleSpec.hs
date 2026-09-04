{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.ModuleSpec where

import AST.Nodes
  ( Class (..),
    Function (..),
    FunctionArg (..),
    FunctionName (..),
    Literal (..),
    Module (..),
    Stmt (..),
  )
import AST.TypeIdentifier (TIdentifier (TIdentifier), fromIdentifier)
import AST.TypeReference (TypeRef (TypeRef, tRefName, tRefType))
import AST.Types (FixType, ResolvedAst, Type, UnresolvedAst)
import Data.Either (fromRight)
import Data.String.Interpolate (__i)
import Parser (parseModule)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolution.Resolver (resolveAst)

spec :: Spec
spec = do
  describe "Parse Modules" $ do
    it "parse a simple module" $
      parse
        parseModule
        ""
        [__i|
      module M
        def foo(x)
          1
        end
      end
      |]
        `shouldParse` Module
          { moduleName = TIdentifier "M",
            moduleMethods =
              [ Function
                  { funName = FunctionName "foo",
                    funArgs =
                      [ FunctionArg
                          { argName = "x",
                            argTypeRestriction = Nothing,
                            argDefaultValue = Nothing
                          }
                      ],
                    funFreeVars = Nothing,
                    funBody = ["1"],
                    funAnnotation = Nothing
                  }
              ]
          }
