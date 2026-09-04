{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.FunctionSpec where

import AST.Nodes
  ( Function (..),
    FunctionAnnotation (..),
    FunctionArg (..),
    FunctionName (..),
    Literal (..),
    TIdentifier (..),
    TypeRef (..),
  )
import AST.Types (FixType, ResolvedAst, Type, UnresolvedAst)
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
import TypeResolution.Resolver (resolveAst)

spec :: Spec
spec = do
  describe "parseFunctions" $
    let runFunctionArgParser = parse parseFunctionArg ""
        runFunctionParser = parse parseFunction ""
     in do
          it "parses an argument with no type and no default value" $
            runFunctionArgParser "x"
              `shouldParse` FunctionArg
                { argName = "x",
                  argType = Nothing,
                  argDefaultValue = Nothing
                }

          it "parses an argument with type and no default value" $
            runFunctionArgParser "x : SomeType"
              `shouldParse` FunctionArg
                { argName = "x",
                  argType = Just (TypeRef {tRefName = TIdentifier "SomeType", tRefType = ()}),
                  argDefaultValue = Nothing
                }

          it "parses an argument with _ type" $
            runFunctionArgParser "x : _"
              `shouldParse` FunctionArg
                { argName = "x",
                  argType = (Just (TypeRef {tRefName = TIdentifier "_", tRefType = ()})),
                  argDefaultValue = Nothing
                }

          it "parses an argument with type and default value" $
            runFunctionArgParser "x : Int32 = 123"
              `shouldParse` FunctionArg
                { argName = "x",
                  argType = (Just (TypeRef {tRefName = TIdentifier "Int32", tRefType = ()})),
                  argDefaultValue = (Just $ LitInt 123)
                }

          it "parses an argument with no type and with default value" $
            runFunctionArgParser "x = 123"
              `shouldParse` FunctionArg
                { argName = "x",
                  argType = Nothing,
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
                                { funName = FunctionName "foo",
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
                                { funName = FunctionName "foo",
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
                                { funName = FunctionName "foo",
                                  funArgs = [],
                                  funFreeVars = Nothing,
                                  funBody = ["1"],
                                  funAnnotation = Just (FunctionAnnotation "SomeAnnotation")
                                }
                            )

          it "parses a function with one arg" $
            runFunctionParser "def foo(x) end"
              `shouldParse` ( Function
                                { funName = FunctionName "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argType = Nothing,
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
                                { funName = FunctionName "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argType = (Just (TypeRef {tRefName = TIdentifier "T", tRefType = ()})),
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
                                { funName = FunctionName "foo",
                                  funArgs =
                                    [ FunctionArg
                                        { argName = "x",
                                          argType = (Just (TypeRef {tRefName = TIdentifier "T", tRefType = ()})),
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
                                { funName = FunctionName "foo",
                                  funArgs =
                                    [ FunctionArg {argName = "x", argType = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "y", argType = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "z", argType = Nothing, argDefaultValue = Nothing}
                                    ],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )

          it "parses a function with multiple args" $
            runFunctionParser "def foo(  x,   y=1  , z :  String =  34) end"
              `shouldParse` ( Function
                                { funName = FunctionName "foo",
                                  funArgs =
                                    [ FunctionArg {argName = "x", argType = Nothing, argDefaultValue = Nothing},
                                      FunctionArg {argName = "y", argType = Nothing, argDefaultValue = (Just $ LitInt 1)},
                                      FunctionArg
                                        { argName = "z",
                                          argType = (Just (TypeRef {tRefName = TIdentifier "String", tRefType = ()})),
                                          argDefaultValue = (Just $ LitInt 34)
                                        }
                                    ],
                                  funFreeVars = Nothing,
                                  funBody = [],
                                  funAnnotation = Nothing
                                }
                            )
