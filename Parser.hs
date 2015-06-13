module Parser where
import Types
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import GHC.Float
import Control.Monad
import Control.Monad.Except

-- Exported Parsing Functions

lispParse :: String -> ThrowsError Val
lispParse = parser' parseExpr

lispParseList :: String -> ThrowsError [Val]
lispParseList = parser' (endBy parseExpr spaces)

-- Parsing Functions

parseExpr :: Parser Val
parseExpr = parseAtom
         <|> try parsePrimitiveAtom
         <|> parseString
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> do
           _ <- char '('
           x <- try parseList <|> parseDottedList
           _ <- char ')'
           return x
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseChar

parseAtom :: Parser Val
parseAtom = do
  f <- letter <|> initialSymbol
  r <- many (letter <|> symbol <|> digit)
  return $ Atom (f:r)

parsePrimitiveAtom :: Parser Val
parsePrimitiveAtom = liftM Atom $ try (string "...") <|> string "+" <|> string "-"

parseString :: Parser Val
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  _ <- char '"'
  return $ String x

parseQuoted :: Parser Val
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser Val
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser Val
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseList :: Parser Val
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser Val
parseDottedList = do
  lHead <- endBy parseExpr spaces
  lTail <- char '.' >> spaces >> parseExpr
  return $ DottedList lHead lTail

parseFloat :: Parser Val
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float $ rFloat x y
  where
    rFloat x y = fst . head $ readFloat $ x ++ "." ++ y

parseNumber :: Parser Val
parseNumber = liftM (Number . read) $ many1 digit

parseBool :: Parser Val
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return  (Bool False))

parseChar :: Parser Val
parseChar = do
  _ <- try $ string "#\\"
  value <- try (string "newline" <|> string "space")
          <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ Char $ case value of
    "space" -> ' '
    "newline" -> '\n'
    _ -> head value

-- Helper Functions

parser' :: Parser a -> String -> ThrowsError a
parser' parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parse err
  Right val -> return val

symbol :: Parser Char
symbol = initialSymbol <|> oneOf "+-.@"

spaces :: Parser ()
spaces = skipMany1 space

initialSymbol :: Parser Char
initialSymbol = oneOf "!$%&*/|:<=>?^_~"

escapedChars :: Parser Char
escapedChars =  do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> x

toDouble :: Val -> Maybe Double
toDouble(Number n) = Just $ fromIntegral n
toDouble(Float f) = Just $ float2Double f
toDouble(_) = Nothing
