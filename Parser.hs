module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readFloat, readHex, readOct)
import Data.Char (toLower)
import Data.Complex
import Data.Ratio
import Data.Array

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             | Ratio Rational
             | Complex (Complex Float)
             | Vector (Array Int LispVal)

instance Show LispVal where
  show = showVal

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
       Left err -> "No match: " ++ show err
       Right val -> "Found " ++ show val

spaces1 :: Parser ()
spaces1 = skipMany1 space

spaces :: Parser ()
spaces = skipMany space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = try $ do
  string "#"
  x <- oneOf "tf"
  return $ case x of
                't' -> Bool True
                'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseComplex
          <|> parseFloat
          <|> parseRatio
          <|> parseNumber
          <|> parseBool
          <|> parseCharacter
          <|> parseQuoted
          <|> try (do
                    string "#("
                    x <- parseVector
                    char ')'
                    return x)
          <|> parseAnyList
          <|> parseQuasiQuote
          <|> parseUnquote


-- ****************************** Exercise 3.3.1 ******************************

parseNumber' :: Parser LispVal
parseNumber' = do
  numberString <- many1 digit
  return $ (Number . read) numberString

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \numberString ->
                return $ (Number . read) numberString

-- ****************************************************************************

-- ***************************** Exercise 3.3.2,3 *****************************

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
                '\\' -> x
                '"'  -> x
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

-- ****************************************************************************

-- ****************************** Exercise 3.3.4 ******************************

parseNumber :: Parser LispVal
parseNumber = do
  number <- (parseDec
         <|> parseDec'
         <|> parseOct
         <|> parseHex
         <|> parseBin)
  return number

parseDec :: Parser LispVal
parseDec = do
  x <- many1 digit
  (return . Number . read) x

parseDec' :: Parser LispVal
parseDec' = try $ do
  -- you have to put try for backtracking char # so the next parser starts
  -- from the beginning
  string "#d"
  x <- many1 digit
  (return . Number . read) x

parseOct :: Parser LispVal
parseOct = try $ do
  string "#o"
  x <- many1 octDigit
  (return . Number . fst . head . readOct) x

parseHex :: Parser LispVal
parseHex = try $ do
  string "#x"
  x <- many1 hexDigit
  (return . Number . fst . head . readHex) x

parseBin :: Parser LispVal
parseBin = try $ do
  string "#b"
  x <- many1 $ oneOf "01"
  (return . Number . readBin) x

readBin :: String -> Integer
readBin = readBin' 0

readBin' dig "" = dig
readBin' dig (x:xs) = readBin' acc xs
  where
    acc = 2 * dig + (if x == '0' then 0 else 1)

-- ****************************************************************************

-- ****************************** Exercise 3.3.5 ******************************

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  x <- try (string "space" <|> string "newline")
   <|> parseSingleCharacter
  return $ Character $ case x of
       "space" -> ' '
       "newline" -> '\n'
       otherwise -> head x

parseSingleCharacter :: Parser [Char]
parseSingleCharacter = do
  x <- anyChar
  notFollowedBy alphaNum
  return [x]

-- ****************************************************************************

-- ****************************** Exercise 3.3.6 ******************************

parseFloat :: Parser LispVal
parseFloat = try $ do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float $ (fst . head . readFloat) (x ++ "." ++ y)

-- ****************************************************************************

-- ****************************** Exercise 3.3.7 ******************************

parseRatio :: Parser LispVal
parseRatio = try $ do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio $ (read x) % (read y)

parseComplex :: Parser LispVal
parseComplex = try $ do
  x <- parseFloat <|> parseDec
  char '+'
  y <- parseFloat <|> parseDec
  char 'i'
  return $ Complex $ toFloat x :+ toFloat y

toFloat :: LispVal -> Float
toFloat (Float f) = f
toFloat (Number n) = fromIntegral n

-- ****************************************************************************

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces1

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces1
  tail <- char '.' >> spaces1 >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- ****************************** Exercise 3.4.1 ******************************

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- ****************************************************************************

-- ****************************** Exercise 3.4.2 ******************************

parseVector :: Parser LispVal
parseVector = do
  values <- sepBy parseExpr spaces1
  return $ Vector (listArray (0, (length values - 1)) values)

-- ****************************************************************************

-- ****************************** Exercise 3.4.3 ******************************

parseAnyList :: Parser LispVal
parseAnyList = do
  char '(' >> spaces
  head <- parseExpr `sepEndBy` spaces
  do
    char '.' >> spaces
    tail <- parseExpr
    spaces >> char ')'
    return $ DottedList head tail
    <|> (spaces >> char ')' >> (return $ List head))

-- ****************************************************************************


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++
  unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Float contents) = show contents
showVal (Ratio contents) = show (numerator contents) ++
  "/" ++ show (denominator contents)
showVal (Complex contents) = show (realPart contents) ++
  "+" ++ show (imagPart contents) ++ "i"
showVal (Vector contents) = "#(" ++ unwordsVector contents ++ ")"
showVal (Character contents) = "#\\" ++ [contents]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector :: Array Int LispVal -> String
unwordsVector = unwordsList . elems

