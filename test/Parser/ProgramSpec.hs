{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.ProgramSpec where

import AstTypes (Class (..), Function (..), FunctionArg (..), FunctionName (..), Module (..), Stmt (..), TIdentifier (..), TypeRef (..))
import Data.Either (fromRight)
import Data.String.Interpolate (__i)
import Parser (parseClass, parseProgram)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)
import TypeResolver (Fix (..), FixType, ResolvedAst, Type (..), UnresolvedAst, resolveAst)

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
                      [ (FunctionArg {argName = "x", argType = Nothing, argDefaultValue = Nothing})
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
                                              argType = Just (TypeRef {tRefName = TIdentifier "Int32", tRefType = ()}),
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
                                      argType = Nothing,
                                      argDefaultValue = Nothing
                                    },
                                  FunctionArg
                                    { argName = "y",
                                      argType = Nothing,
                                      argDefaultValue = Nothing
                                    }
                                ],
                              funFreeVars = Nothing,
                              funBody = ["x + 34"],
                              funAnnotation = Nothing
                            }
                      ]
