{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec where

import AstTypes
  ( Class (..),
    Function (..),
    FunctionArg (..),
    Literal (..),
    Stmt (..),
  )
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
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)

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

  describe "parseFunctions" $
    let runFunctionArgParser = parse parseFunctionArg ""
        runFunctionParser = parse parseFunction ""
     in do
          it "parses an argument with no type and no default value" $
            runFunctionArgParser "x"
              `shouldParse` FunctionArg
                { argName = "x",
                  argTypeName = Nothing,
                  argDefaultValue = Nothing
                }

          it "parses an argument with type and no default value" $
            runFunctionArgParser "x : SomeType"
              `shouldParse` FunctionArg
                { argName = "x",
                  argTypeName = (Just "SomeType"),
                  argDefaultValue = Nothing
                }

          it "parses an argument with _ type" $
            runFunctionArgParser "x : _"
              `shouldParse` FunctionArg
                { argName = "x",
                  argTypeName = (Just "_"),
                  argDefaultValue = Nothing
                }

          it "parses an argument with type and default value" $
            runFunctionArgParser "x : Int32 = 123"
              `shouldParse` FunctionArg
                { argName = "x",
                  argTypeName = (Just "Int32"),
                  argDefaultValue = (Just $ LitInt 123)
                }

          it "parses an argument with no type and with default value" $
            runFunctionArgParser "x = 123"
              `shouldParse` FunctionArg
                { argName = "x",
                  argTypeName = Nothing,
                  argDefaultValue = (Just $ LitInt 123)
                }

          it "parses a function with no args" $
            runFunctionParser
              [__i|
              def foo()
                hola lulu
                chau lulu
              end
              |]
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs = [],
                                  funFreeVars = Nothing,
                                  funBody = ["hola lulu", "chau lulu"],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with no content" $
            parse
              parseFunction
              ""
              [__i|
              def foo()
              end
              |]
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs = [],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a trivial function with annotation" $
            runFunctionParser
              [__i|
              @[SomeAnnotation]
              def foo()
                1
              end
              |]
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs = [],
                                  funFreeVars = Nothing,
                                  funBody = ["1"],
                                  funAnnotation = Just "SomeAnnotation"
                                }
                            )

          it "parses a function with one arg" $
            runFunctionParser "def foo(x) end"
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argTypeName = Nothing,
                                          argDefaultValue = Nothing
                                        }
                                    ],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with multiple forall args" $
            runFunctionParser "def foo(x : T) forall T, S, R end"
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argTypeName = (Just "T"),
                                          argDefaultValue = Nothing
                                        }
                                    ],
                                  funFreeVars = (Just ["T", "S", "R"]),
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with one forall arg" $
            runFunctionParser "def foo(x : T) forall T end"
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argTypeName = (Just "T"),
                                          argDefaultValue = Nothing
                                        }
                                    ],
                                  funFreeVars = (Just ["T"]),
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with multiple args" $
            runFunctionParser "def foo(  x,   y  , z) end"
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs =
                                    [ FunctionArg {argName = "x", argTypeName = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "y", argTypeName = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "z", argTypeName = Nothing, argDefaultValue = Nothing}
                                    ],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with multiple args" $
            runFunctionParser "def foo(  x,   y=1  , z :  String =  34) end"
              `shouldParse` ( Function
                                { funName = "foo",
                                  funArgs =
                                    [ FunctionArg {argName = "x", argTypeName = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "y", argTypeName = Nothing, argDefaultValue = (Just $ LitInt 1)},
                                      FunctionArg {argName = "z", argTypeName = (Just "String"), argDefaultValue = (Just $ LitInt 34)}
                                    ],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

  describe "parseClasses" $ do
    it "parse an empty class" $
      parse parseClass "" "class A end"
        `shouldParse` Class {className = "A", classSuper = "Reference", classMethods = [], classModules = []}

    it "parse a class with one method" $
      parse
        parseClass
        ""
        [__i|
        class A
          def foo()
            1
          end
        end
        |]
        `shouldParse` Class
          { className = "A",
            classSuper = "Reference",
            classMethods =
              [ Function {funName = "foo", funArgs = [], funFreeVars = Nothing, funBody = ["1"], funAnnotation = Nothing}
              ],
            classModules = []
          }

    it "parse a class with superclass" $
      parse
        parseClass
        ""
        [__i|
        class A < Bar
          def foo()
            1
          end
        end
        |]
        `shouldParse` Class
          { className = "A",
            classSuper = "Bar",
            classMethods =
              [ Function {funName = "foo", funArgs = [], funFreeVars = Nothing, funBody = ["1"], funAnnotation = Nothing}
              ],
            classModules = []
          }

    it "parses a class with multiple methods" $
      parse
        parseClass
        ""
        [__i|
        class A
          def foo()
            1
          end
          def foo(x, y)
            2
          end
          def foo(x, y = 1)
            3
          end
        end
        |]
        `shouldParse` Class
          { className = "A",
            classSuper = "Reference",
            classModules = [],
            classMethods =
              [ Function
                  { funName = "foo",
                    funArgs = [],
                    funFreeVars = Nothing,
                    funBody = ["1"],
                    funAnnotation = Nothing
                  },
                Function
                  { funName = "foo",
                    funArgs =
                      [ FunctionArg {argName = "x", argTypeName = Nothing, argDefaultValue = Nothing},
                        FunctionArg {argName = "y", argTypeName = Nothing, argDefaultValue = Nothing}
                      ],
                    funFreeVars = Nothing,
                    funBody = ["2"],
                    funAnnotation = Nothing
                  },
                Function
                  { funName = "foo",
                    funArgs =
                      [ FunctionArg {argName = "x", argTypeName = Nothing, argDefaultValue = Nothing},
                        FunctionArg {argName = "y", argTypeName = Nothing, argDefaultValue = (Just $ LitInt 1)}
                      ],
                    funFreeVars = Nothing,
                    funBody = ["3"],
                    funAnnotation = Nothing
                  }
              ]
          }

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
          { className = "A",
            classSuper = "Reference",
            classMethods =
              [ Function
                  { funName = "foo",
                    funArgs =
                      [ (FunctionArg {argName = "x", argTypeName = Nothing, argDefaultValue = Nothing})
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
                            { className = "A",
                              classSuper = "Reference",
                              classMethods =
                                [ Function
                                    { funName = "foo",
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
                            { className = "B",
                              classSuper = "A",
                              classMethods =
                                [ Function
                                    { funName = "foo",
                                      funArgs =
                                        [ FunctionArg
                                            { argName = "x",
                                              argTypeName = Just "Int32",
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
                            { className = "A",
                              classSuper = "Reference",
                              classMethods =
                                [ Function
                                    { funName = "foo",
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
                            { funName = "baz",
                              funArgs =
                                [ FunctionArg
                                    { argName = "x",
                                      argTypeName = Nothing,
                                      argDefaultValue = Nothing
                                    },
                                  FunctionArg
                                    { argName = "y",
                                      argTypeName = Nothing,
                                      argDefaultValue = Nothing
                                    }
                                ],
                              funFreeVars = Nothing,
                              funBody = ["x + 34"],
                              funAnnotation = Nothing
                            }
                      ]
