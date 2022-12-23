{-# LANGUAGE RankNTypes #-}

module Parsers.Common
  ( Parser
  , lexeme
  , identifier
  , identifier0
  , label
  , string
  , char'
  , natural
  , limitedNatural
  , integer
  , limitedInteger
  , stringLiteral
  , hexLiteral
  , comma
  , whiteSpace
  , comment
  -- , space
  , hspace
  , hspace1
  , lineEnd
  , eol
  , eof
  )
where

import Text.Megaparsec hiding ( label )
import Text.Megaparsec.Char hiding ( string, space )
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Functor.Identity

import Data.Foldable
import Data.Char
import Control.Monad
import Data.Void
import Data.Text (Text, pack, unpack)

import Data.Bits

type Parser = Parsec Void String

whiteSpace :: Parser ()
whiteSpace = L.space hspace1 comment  empty

comment :: Parser ()
comment = L.skipLineComment "."

hspacenl :: Parser ()
hspacenl = try (hspace1 <* optional lookEOL) <|> lookEOL
  where
    isHSpace x  = isSpace x && x /= '\n' && x /= '\r'
    lookEOL     = void $ try . lookAhead $ eol

space :: Parser ()
space = L.space space1 (L.skipLineComment ".") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

identifier :: Parser String
identifier = do
  c <- identStart
  cs <- many identRest
  whiteSpace
  pure $ map toUpper (c:cs)

identifier0 :: Parser String
identifier0 = try identifier <|> (whiteSpace >> pure "")

label :: Parser String
label = try label' <|> (whiteSpace >> pure "")
  where
    label' = do
      c <- identStart
      cs <- many identRest
      space
      pure $ map toUpper (c:cs)

identStart :: Parser Char
identStart = letterChar <|> char '_'

identRest :: Parser Char
identRest = alphaNumChar <|> char '_'

symbol :: String -> Parser String
symbol = L.symbol' whiteSpace

string :: String -> Parser String
string = string'

lineEnd :: Parser ()
lineEnd = void eol <|> eof

stringLiteral :: Parser String
stringLiteral = do
  quote <- satisfy (\x -> x == '\'' || x == '"')
  manyTill asciiChar (char quote)

hexLiteral :: Parser String
hexLiteral = do
  quote <- satisfy (\x -> x == '\'' || x == '"')
  manyTill hexDigitChar (char quote)

binary      :: Parser Int
hexadecimal :: Parser Int
octal       :: Parser Int
decimal     :: Parser Int
binary      = char '0' >> char' 'b' >> L.binary
octal       = char '0' >> char' 'o' >> L.octal
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal
decimal     = L.decimal

natural :: Parser Word
natural = fromIntegral <$> choice
  [ try binary
  , try octal
  , try hexadecimal
  , decimal
  ] <?> "natural"

limitedNatural :: Int -> Parser Word
limitedNatural bitCount = do
  n <- natural
  if countLeadingZeros n - finiteBitSize n <= bitCount
    then pure n
    else fail ("Number " ++ show n ++ " out of range (max size: 2^" ++ show bitCount ++ ")")

signed :: (Num a) => Parser a -> Parser a
signed = L.signed (void $ string "")

integer :: Parser Int
integer = choice
  [ try (signed binary)
  , try (signed octal)
  , try (signed hexadecimal)
  , signed decimal
  ] <?> "integer"

limitedInteger :: Int -> Parser Int
limitedInteger bitCount = do
  n <- integer
  if n >  0 && countLeadingZeros n - finiteBitSize n <= bitCount ||
     n <= 0 && n >= -2^bitCount
    then pure n
    else fail ("Number " ++ show n ++ " out of range (max size: 2^" ++ show bitCount ++ ")")

comma :: Parser Char
comma = lexeme $ char ','