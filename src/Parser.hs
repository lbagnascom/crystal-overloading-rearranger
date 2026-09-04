{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST.Nodes
  ( Callsite (Callsite, callsiteArgs, callsiteFunName),
    Class (Class, classMethods, classModules, className, classSuper),
    Expr (ECall, ELiteral, ENew),
    Function (Function, funAnnotation, funArgs, funBody, funFreeVars, funName),
    FunctionAnnotation (FunctionAnnotation),
    FunctionArg (FunctionArg, argDefaultValue, argName, argTypeRestriction),
    FunctionName (FunctionName),
    Literal (LitBool, LitInt, LitString),
    Module (Module, moduleMethods, moduleName),
    Stmt (ClassStmt, ExprStmt, FunctionStmt, ModuleStmt),
  )
import AST.TypeIdentifier (TIdentifier (TIdentifier))
import AST.TypeReference (TypeRef (TypeRef, tRefName, tRefType))
import AST.TypeRestriction (TypeRestriction (TResType))
import AST.Types (UnresolvedAst, UnresolvedStmt, UnresolvedType)
import Data.Char (isSpace)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    anySingle,
    between,
    choice,
    empty,
    many,
    manyTill,
    option,
    optional,
    sepBy,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    upperChar,
  )
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

parseFunctionArg :: Parser (FunctionArg UnresolvedType)
parseFunctionArg = do
  varName <- parseVarName
  funArgType <- optional (symbol ":" *> (unresolvedTypeRestriction <$> parseTypeName))
  defaultValue <- optional $ symbol "=" *> parseLiteral
  pure $
    FunctionArg {argName = varName, argTypeRestriction = funArgType, argDefaultValue = defaultValue}

parseFunction :: Parser (Function UnresolvedType)
parseFunction = do
  annotation <- optional $ between (symbol "@[") (symbol "]") (FunctionAnnotation <$> parseCapitalizedName)
  _ <- symbol "def"
  name <- FunctionName <$> parseVarName
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

parseModule :: Parser (Module UnresolvedType)
parseModule = do
  _ <- symbol "module"
  name <- parseCapitalizedName
  defs <- manyTill parseFunction (symbol "end")
  pure $ Module {moduleName = TIdentifier name, moduleMethods = defs}

-- Classes

unresolvedTypeRef :: String -> TypeRef UnresolvedType
unresolvedTypeRef name = TypeRef {tRefName = TIdentifier name, tRefType = ()}

unresolvedTypeRestriction :: String -> TypeRestriction UnresolvedType
unresolvedTypeRestriction = TResType . unresolvedTypeRef

parseClass :: Parser (Class UnresolvedType)
parseClass = do
  _ <- symbol "class"
  name <- TIdentifier <$> parseCapitalizedName
  super <- unresolvedTypeRef <$> option "Reference" (symbol "<" *> parseCapitalizedName)
  includes <- many (symbol "include" *> (unresolvedTypeRef <$> parseCapitalizedName))
  defs <- manyTill parseFunction (symbol "end")
  pure $ Class {className = name, classSuper = super, classModules = includes, classMethods = defs}

-- Expressions

parseExpr :: Parser (Expr UnresolvedType)
parseExpr =
  choice
    [ ELiteral <$> parseLiteral,
      ENew <$> parseNew,
      ECall <$> parseCall
    ]

parseCall :: Parser (Callsite UnresolvedType)
parseCall = do
  funname <- FunctionName <$> parseVarName
  args <- between (symbol "(") (symbol ")") (parseExpr `sepBy` symbol ",")
  pure $ Callsite {callsiteFunName = funname, callsiteArgs = args}

parseNew :: Parser (TypeRef UnresolvedType)
parseNew = do
  name <- TIdentifier <$> parseCapitalizedName <* symbol ".new"
  -- TODO: support constructors that take arguments
  pure $ TypeRef {tRefName = name, tRefType = ()}

-- Crystal program

parseStmt :: Parser UnresolvedStmt
parseStmt =
  choice
    [ ClassStmt <$> parseClass,
      ModuleStmt <$> parseModule,
      FunctionStmt <$> parseFunction,
      ExprStmt <$> parseExpr
    ]

parseProgram :: Parser UnresolvedAst
parseProgram = manyTill parseStmt eof
