{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AstTypes
import Data.Char (isSpace)
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TypeResolver (UnresolvedAst, UnresolvedStmt)

type Parser = Parsec Void Text

-- Space consuming

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Literal

parseLiteral :: Parser Literal
parseLiteral = parseInteger <|> parseString <|> parseBool

parseInteger :: Parser Literal
parseInteger = LitInt <$> lexeme L.decimal

parseString :: Parser Literal
parseString = LitString <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

parseBool :: Parser Literal
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = LitBool True <$ symbol "true"
    parseFalse = LitBool False <$ symbol "false"

-- Functions / Methods

parseVarName :: Parser String
parseVarName = lexeme $ do
  nameHead <- letterChar
  nameTail <- many alphaNumChar
  pure (nameHead : nameTail)

parseTypeName :: Parser String
parseTypeName = (unpack <$> symbol "_") <|> parseCapitalizedName

parseCapitalizedName :: Parser String
parseCapitalizedName = lexeme $ do
  nameHead <- upperChar
  nameTail <- many alphaNumChar
  pure (nameHead : nameTail)

parseFunctionArg :: Parser (FunctionArg String)
parseFunctionArg = do
  varName <- parseVarName
  funArgType <- optional (symbol ":" *> parseTypeName)
  defaultValue <- optional $ symbol "=" *> parseLiteral
  pure $
    FunctionArg
      { argName = varName,
        argTypeName = funArgType,
        argDefaultValue = defaultValue
      }

parseFunction :: Parser (Function String)
parseFunction = do
  annotation <- optional $ between (symbol "@[") (symbol "]") parseCapitalizedName
  _ <- symbol "def"
  name <- parseVarName
  args <- between (symbol "(") (symbol ")") (parseFunctionArg `sepBy` symbol ",")
  freeVars <- optional $ symbol "forall" *> (parseCapitalizedName `sepBy` symbol ",")
  body <- filter (not . null) . map (dropWhile isSpace) . lines <$> manyTill anySingle (symbol "end")
  pure $
    Function
      { funName = name,
        funArgs = args,
        funFreeVars = freeVars,
        funBody = body,
        funAnnotation = annotation
      }

-- Modules

parseModule :: Parser (Module String)
parseModule = do
  _ <- symbol "module"
  name <- parseCapitalizedName
  defs <- manyTill parseFunction (symbol "end")
  pure $ Module {moduleName = name, moduleMethods = defs}

-- Classes

parseClass :: Parser (Class String)
parseClass = do
  _ <- symbol "class"
  name <- parseCapitalizedName
  super <- option "Reference" (symbol "<" *> parseCapitalizedName)
  includes <- many (symbol "include" *> parseCapitalizedName)
  defs <- manyTill parseFunction (symbol "end")
  pure $ Class {className = name, classSuper = super, classModules = includes, classMethods = defs}

-- Crystal program

parseStmt :: Parser UnresolvedStmt
parseStmt =
  choice
    [ ClassStmt <$> parseClass,
      ModuleStmt <$> parseModule,
      FunctionStmt <$> parseFunction,
      UndiscoveredStmt <$> manyTill anySingle eol
    ]

parseProgram :: Parser UnresolvedAst
parseProgram = manyTill parseStmt eof
