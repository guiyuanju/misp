module Parser
  ( readExpr,
  )
where

import Model
import Text.ParserCombinators.Parsec hiding (spaces)

specialSymbol :: Parser Char
specialSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

spaces :: Parser ()
spaces = skipMany space

parseNumber :: Parser Expr
parseNumber = Number . read <$> many1 digit

parseString :: Parser Expr
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseComment :: Parser ()
parseComment = many1 (char ';') >> many (noneOf "\n") >> return ()

parseSymbolString :: Parser String
parseSymbolString = do
  first <- specialSymbol <|> letter
  rest <- many $ specialSymbol <|> letter <|> digit
  return $ first : rest

parseSymbol :: Parser Expr
parseSymbol = Symbol <$> parseSymbolString

parseListRaw :: Parser [Expr]
parseListRaw = sepEndBy parseExpr spaces1

parseList :: Parser Expr
parseList = List <$> parseListRaw

parseFunction :: Parser Expr
parseFunction = do
  _ <- string "lambda"
  spaces
  _ <- char '('
  spaces
  args <- sepBy parseSymbolString spaces1
  spaces
  _ <- char ')'
  spaces
  body <- parseProgram
  return $ Function "lambda" (Env []) (args, body)

parseDef :: Parser Expr
parseDef = do
  _ <- string "def"
  spaces1
  name <- parseSymbolString
  spaces1
  Def name <$> parseExpr

parseDefSymbol :: Parser Expr
parseDefSymbol = do
  _ <- string "def"
  spaces1
  name <- parseSymbolString
  spaces1
  Def name <$> parseExpr

-- parseDefFunction :: Parser Expr
-- parseDefFunction = do
--   _ <- string "def"
--   spaces1
--   leftParen
--   params <- sepBy parseSymbolString spaces1
--   rightParen
--   spaces1

parseSet :: Parser Expr
parseSet = do
  _ <- string "set"
  spaces1
  name <- parseSymbolString
  spaces1
  Set name <$> parseExpr

parseIf :: Parser Expr
parseIf = do
  _ <- string "if"
  spaces1
  cond <- parseExpr
  spaces1
  trueBranch <- parseExpr
  spaces1
  If cond trueBranch <$> parseExpr

parseBool :: Parser Expr
parseBool = do
  b <- string "true" <|> string "false"
  return $ Boolean (b == "true")

parseSymbolOrBool :: Parser Expr
parseSymbolOrBool = do
  s <- parseSymbol
  case s of
    Symbol "true" -> return $ Boolean True
    Symbol "false" -> return $ Boolean False
    Symbol "nil" -> return $ List []
    s -> return s

parseProg :: Parser Expr
parseProg = do
  _ <- string "prog"
  spaces1
  Prog <$> parseListRaw

parseWhile :: Parser Expr
parseWhile = do
  _ <- string "while"
  spaces1
  cond <- parseExpr
  spaces1
  body <- parseListRaw
  return $ While cond body

parseExpr :: Parser Expr
parseExpr =
  parseNumber
    <|> parseString
    <|> parseSymbolOrBool
    <|> do
      _ <- char '('
      spaces
      x <-
        try parseIf
          <|> try parseSet
          <|> try parseDef
          <|> try parseFunction
          <|> try parseProg
          <|> try parseWhile
          <|> parseList
      spaces
      _ <- char ')'
      return x

data ExprOrComment = ECExpr Expr | ECComment ()

parseExprOrComment :: Parser ExprOrComment
parseExprOrComment = (ECExpr <$> parseExpr) <|> (ECComment <$> parseComment)

parseProgram :: Parser Program
parseProgram =
  Program
    <$> fmap castMaybe (sepEndBy parseExprOrComment spaces1)
  where
    castMaybe xs = [x | ECExpr x <- xs]

readExpr :: String -> Either ParseError Program
readExpr = parse parseProgram "lisp"
