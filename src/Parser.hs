{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (isSpace)
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

-- Literals

data Literal
  = CrString String
  | CrInt Integer
  | CrBool Bool
  deriving (Show, Eq)

parseLiteral :: Parser Literal
parseLiteral = parseInteger <|> parseString <|> parseBool

parseInteger :: Parser Literal
parseInteger = CrInt <$> lexeme L.decimal

parseString :: Parser Literal
parseString = CrString <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

parseBool :: Parser Literal
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = CrBool True <$ symbol "true"
    parseFalse = CrBool False <$ symbol "false"

-- Functions / Methods

data Function = Function
  { funName :: String,
    funArgs :: [FunctionArg],
    funFreeVars :: Maybe [String],
    funBody :: [String],
    funAnnotation :: Maybe String
  }
  deriving (Show, Eq)

data FunctionArg = FunctionArg
  { argName :: String,
    argType :: Maybe String,
    argDefaultValue :: Maybe Literal
  }
  deriving (Show, Eq)

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

parseFunctionArg :: Parser FunctionArg
parseFunctionArg = do
  varName <- parseVarName
  funArgType <- optional (symbol ":" *> parseTypeName)
  defaultValue <- optional $ symbol "=" *> parseLiteral
  pure $
    FunctionArg
      { argName = varName,
        argType = funArgType,
        argDefaultValue = defaultValue
      }

parseFunction :: Parser Function
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

data Module = Module
  { moduleName :: String,
    moduleMethods :: [Function]
  }
  deriving (Show, Eq)

parseModule :: Parser Module
parseModule = do
  _ <- symbol "module"
  name <- parseCapitalizedName
  defs <- manyTill parseFunction (symbol "end")
  pure $ Module {moduleName = name, moduleMethods = defs}

-- Classes

data Class = Class
  { className :: String,
    classSuper :: String,
    classModules :: [Module],
    classMethods :: [Function]
  }
  deriving (Show, Eq)

parseClass :: Parser Class
parseClass = do
  _ <- symbol "class"
  name <- parseCapitalizedName
  super <- option "Reference" (symbol "<" *> parseCapitalizedName)
  includes <- manyTill (symbol "include" *> parseCapitalizedName) eol
  defs <- manyTill parseFunction (symbol "end")
  pure $ Class {className = name, classSuper = super, classModules = includes, classMethods = defs}

-- Crystal program

data Stmt
  = ClassStmt Class
  | ModuleStmt Module
  | FunctionStmt Function
  | UndiscoveredStmt String
  deriving (Show, Eq)

parseStmt :: Parser Stmt
parseStmt =
  choice
    [ ClassStmt <$> parseClass,
      ModuleStmt <$> parseModule,
      FunctionStmt <$> parseFunction,
      UndiscoveredStmt <$> manyTill anySingle eol
    ]

type CrystalProgram = [Stmt]

parseProgram :: Parser CrystalProgram
parseProgram = manyTill parseStmt eof
