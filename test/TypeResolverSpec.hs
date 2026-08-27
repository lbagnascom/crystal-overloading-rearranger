{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeResolverSpec where

import AstTypes (Callsite (..), Class (..), Expr (..), Function (..), FunctionArg (..), FunctionName (..), Literal (..), Module (..), Stmt (..), TIdentifier (..), TypeRef (..))
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
import TypeResolver (Fix (..), FixType, ResolvedAst, Type (..), UnresolvedAst, r, resolveAst)

oneSimpleFoo :: UnresolvedAst
oneSimpleFoo =
  fromRight [] $
    parse
      parseProgram
      ""
      [__i|
      def foo(x : Bool)
        1
      end
      |]

simpleFooOverloading :: UnresolvedAst
simpleFooOverloading =
  fromRight [] $
    parse
      parseProgram
      ""
      [__i|
      def foo(x : String)
        "x : String"
      end

      def foo(x : Bool)
        "x : Bool"
      end
      |]

moduleAndClassHierarchy :: UnresolvedAst
moduleAndClassHierarchy =
  fromRight [] $
    parse
      parseProgram
      ""
      [__i|
      module M
      end

      class A
        include M
      end

      class B < A
      end
      |]

spec :: Spec
spec = do
  describe "typeResolver" $
    do
      it "probando type resolver" $
        resolveAst oneSimpleFoo
          `shouldBe` [ FunctionStmt
                         ( Function
                             { funName = FunctionName "foo",
                               funArgs =
                                 [ FunctionArg
                                     { argName = "x",
                                       argType = Just (TypeRef {tRefName = TIdentifier "Bool", tRefType = Fix TBool}),
                                       argDefaultValue = Nothing
                                     }
                                 ],
                               funFreeVars = Nothing,
                               funBody = ["1"],
                               funAnnotation = Nothing
                             }
                         )
                     ]
      it "probando type resolver dado un callsite facil" $
        r
          (resolveAst simpleFooOverloading)
          ( Callsite
              { callsiteFunName = FunctionName "foo",
                callsiteArgs =
                  [ ELiteral $ LitBool True
                  ]
              }
          )
          `shouldBe` [ Function
                         { funName = FunctionName "foo",
                           funArgs =
                             [ FunctionArg
                                 { argName = "x",
                                   argType = Just (TypeRef {tRefName = TIdentifier "Bool", tRefType = Fix TBool}),
                                   argDefaultValue = Nothing
                                 }
                             ],
                           funFreeVars = Nothing,
                           funBody = ["\"x : Bool\""],
                           funAnnotation = Nothing
                         }
                     ]
      it "funciona para clases y modulos" $
        resolveAst moduleAndClassHierarchy
          `shouldBe` [ ModuleStmt
                         ( Module
                             { moduleName = TIdentifier "M",
                               moduleMethods = []
                             }
                         ),
                       ClassStmt
                         ( Class
                             { className = TIdentifier "A",
                               classSuper = TypeRef {tRefName = TIdentifier "Reference", tRefType = Fix (TClass (Class {className = TIdentifier "Reference", classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = Fix TString}, classModules = [], classMethods = []}))},
                               classModules = [TypeRef {tRefName = TIdentifier "M", tRefType = Fix (TModule (Module {moduleName = TIdentifier "M", moduleMethods = []}))}],
                               classMethods = []
                             }
                         ),
                       ClassStmt
                         ( Class
                             { className = TIdentifier "B",
                               classSuper = TypeRef {tRefName = TIdentifier "A", tRefType = Fix (TClass (Class {className = TIdentifier "A", classSuper = TypeRef {tRefName = TIdentifier "Reference", tRefType = Fix (TClass (Class {className = TIdentifier "Reference", classSuper = TypeRef {tRefName = TIdentifier "Object", tRefType = Fix TString}, classModules = [], classMethods = []}))}, classModules = [TypeRef {tRefName = TIdentifier "M", tRefType = Fix (TModule (Module {moduleName = TIdentifier "M", moduleMethods = []}))}], classMethods = []}))},
                               classModules = [],
                               classMethods = []
                             }
                         )
                     ]
