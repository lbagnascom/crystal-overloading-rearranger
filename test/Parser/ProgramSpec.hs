{-# LANGUAGE QuasiQuotes #-}

module Parser.ProgramSpec where

import AST.Nodes
  ( Class (Class, classMethods, classModules, className, classSuper),
    Function (Function, funAnnotation, funArgs, funBody, funFreeVars, funName),
    FunctionArg (FunctionArg, argDefaultValue, argName, argTypeRestriction),
    FunctionName (FunctionName),
    Literal (LitBool, LitInt, LitString),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, ExprStmt, FunctionStmt, ModuleStmt),
  )
import AST.TypeIdentifier (TIdentifier (TIdentifier), fromIdentifier)
import AST.TypeReference (TypeRef (TypeRef, tRefName, tRefType))
import AST.TypeRestriction (TypeRestriction (TResType))
import AST.Types (FixType, ResolvedAst, Type, UnresolvedAst)
import Data.Either (fromRight)
import Data.String.Interpolate (__i)
import Parser (parseClass, parseProgram)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolution.Resolver (resolveAst)

spec :: Spec
spec = do
  describe "parsePrograms" $ do
    it "parse a simple program" $
      parse
        parseClass
        ""
        [__i|
        class A
          def foo(x)
            1
          end
        end
        |]
        `shouldParse` Class
          { className = TIdentifier "A",
            classSuper = (TypeRef {tRefName = TIdentifier "Reference", tRefType = ()}),
            classMethods =
              [ Function
                  { funName = FunctionName "foo",
                    funArgs =
                      [ (FunctionArg {argName = "x", argTypeRestriction = Nothing, argDefaultValue = Nothing})
                      ],
                    funFreeVars = Nothing,
                    funBody = ["1"],
                    funAnnotation = Nothing
                  }
              ],
            classModules = []
          }

    it "parse two classes" $
      parse
        parseProgram
        ""
        [__i|
        class A
          def foo()
            1
          end
        end

        class B < A
          def foo(x : Int32)
            x + 34
          end
        end
        |]
        `shouldParse` [ ClassStmt $
                          Class
                            { className = TIdentifier "A",
                              classSuper = (TypeRef {tRefName = TIdentifier "Reference", tRefType = ()}),
                              classMethods =
                                [ Function
                                    { funName = FunctionName "foo",
                                      funArgs = [],
                                      funFreeVars = Nothing,
                                      funBody = ["1"],
                                      funAnnotation = Nothing
                                    }
                                ],
                              classModules = []
                            },
                        ClassStmt $
                          Class
                            { className = TIdentifier "B",
                              classSuper = (TypeRef {tRefName = TIdentifier "A", tRefType = ()}),
                              classMethods =
                                [ Function
                                    { funName = FunctionName "foo",
                                      funArgs =
                                        [ FunctionArg
                                            { argName = "x",
                                              argTypeRestriction = Just $ TResType (TypeRef {tRefName = TIdentifier "Int32", tRefType = ()}),
                                              argDefaultValue = Nothing
                                            }
                                        ],
                                      funFreeVars = Nothing,
                                      funBody = ["x + 34"],
                                      funAnnotation = Nothing
                                    }
                                ],
                              classModules = []
                            }
                      ]

    it "parses a class and a function" $
      parse
        parseProgram
        ""
        [__i|
        class A
          def foo()
            1
          end
        end

        def baz(x, y)
          x + 34
        end
        |]
        `shouldParse` [ ClassStmt $
                          Class
                            { className = TIdentifier "A",
                              classSuper = TypeRef {tRefName = TIdentifier "Reference", tRefType = ()},
                              classMethods =
                                [ Function
                                    { funName = FunctionName "foo",
                                      funArgs = [],
                                      funFreeVars = Nothing,
                                      funBody = ["1"],
                                      funAnnotation = Nothing
                                    }
                                ],
                              classModules = []
                            },
                        FunctionStmt $
                          Function
                            { funName = FunctionName "baz",
                              funArgs =
                                [ FunctionArg
                                    { argName = "x",
                                      argTypeRestriction = Nothing,
                                      argDefaultValue = Nothing
                                    },
                                  FunctionArg
                                    { argName = "y",
                                      argTypeRestriction = Nothing,
                                      argDefaultValue = Nothing
                                    }
                                ],
                              funFreeVars = Nothing,
                              funBody = ["x + 34"],
                              funAnnotation = Nothing
                            }
                      ]
