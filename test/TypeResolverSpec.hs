{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeResolverSpec where

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
import Parser
  ( parseBool,
    parseClass,
    parseFunction,
    parseFunctionArg,
    parseInteger,
    parseProgram,
    parseString,
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolver
  ( Fix (..),
    FixType,
    ResolvedAst,
    Type (..),
    UnresolvedAst,
    baseTypes,
    isSubclassOf,
    mapType,
    objectClass,
    r,
    referenceClass,
    resolveAst,
  )

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

destroyClass :: FixType -> Class FixType
destroyClass (Fix (TClass c)) = c

resolvedClass :: Class () -> Class FixType
resolvedClass = destroyClass . mapType baseTypes . TClass

resolvedObjectClass :: Class FixType
resolvedObjectClass = resolvedClass objectClass

resolvedReferenceClass :: Class FixType
resolvedReferenceClass = resolvedClass referenceClass

emptyModuleM :: Module FixType
emptyModuleM = Module {moduleName = TIdentifier "M", moduleMethods = []}

classA :: Class FixType
classA =
  Class
    { className = TIdentifier "A",
      classSuper = TypeRef {tRefName = TIdentifier "Reference", tRefType = Fix (TClass resolvedReferenceClass)},
      classModules = [TypeRef {tRefName = TIdentifier "M", tRefType = Fix (TModule emptyModuleM)}],
      classMethods = []
    }

classB :: Class FixType
classB =
  Class
    { className = TIdentifier "B",
      classSuper = TypeRef {tRefName = TIdentifier "A", tRefType = Fix (TClass classA)},
      classModules = [],
      classMethods = []
    }

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
        resolveAst moduleAndClassHierarchy `shouldBe` [ModuleStmt emptyModuleM, ClassStmt classA, ClassStmt classB]
      it "Object es su propia superclase" $
        resolvedObjectClass `shouldSatisfy` (resolvedObjectClass `isSubclassOf`)
      it "Object es super de Reference" $
        resolvedObjectClass `shouldSatisfy` (resolvedReferenceClass `isSubclassOf`)
      it "Object es super de cualquier clase" $
        resolvedObjectClass `shouldSatisfy` (classA `isSubclassOf`)
      it "Reference es super si no se especifica" $
        resolvedReferenceClass `shouldSatisfy` (classA `isSubclassOf`)
      it "B es subclase de A" $
        classB `shouldSatisfy` (`isSubclassOf` classA)
