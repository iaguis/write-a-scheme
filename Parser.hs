module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readFloat, readHex, readOct)
import Data.Char (toLower)
import Data.Complex
import Data.Ratio

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
             deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
       Left err -> "No match: " ++ show err
       Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
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

-- ******************************** Exercise 1 ********************************

parseNumber' :: Parser LispVal
parseNumber' = do
  numberString <- many1 digit
  return $ (Number . read) numberString

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \numberString ->
                return $ (Number . read) numberString

-- ****************************************************************************

-- ******************************** Exercise 2,3 ********************************

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

-- ******************************** Exercise 4 ********************************

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
parseDec' = do
  -- you have to put try for backtracking char # so the next parser starts
  -- from the beginning
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  (return . Number . fst . head . readOct) x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  (return . Number . fst . head . readHex) x

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 $ oneOf "01"
  (return . Number . readBin) x

readBin :: String -> Integer
readBin = readBin' 0

readBin' dig "" = dig
readBin' dig (x:xs) = readBin' acc xs
  where
    acc = 2 * dig + (if x == '0' then 0 else 1)

-- ****************************************************************************

-- ******************************** Exercise 5 ********************************

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

-- ******************************** Exercise 6 ********************************

parseFloat :: Parser LispVal
parseFloat = try $ do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float $ (fst . head . readFloat) (x ++ "." ++ y)

-- ****************************************************************************

-- ******************************** Exercise 7 ********************************

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

parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseComplex
          <|> parseFloat
          <|> parseRatio
          <|> parseNumber
          <|> parseBool

-- Function for testing in GHCi
run parser input =
  case runParser parser () "lisp" input of
       Right n -> "parsed " ++ show n
       Left err -> "error"
