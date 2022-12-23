{-# LANGUAGE RankNTypes #-}

module Parsers.Data ( Data (Bytes, Num), rawData, asciiData, hexData, numData ) where
import Parsers.Common
import Text.Megaparsec
import Text.Megaparsec.Char ( hexDigitChar )
import Text.Megaparsec.Char.Lexer ( hexadecimal )

import Control.Lens
import Data.Bits.Lens
import Data.Word
import Numeric.Lens

data Data 
  = Bytes [Word8]
  | Num Int
  deriving Show

byteArray :: String -> [Word8]
byteArray [] = []
byteArray string = let
  (byte, rest) = splitAt 2 string

  byte' = if length byte < 2
    then (byte ++ "0") ^? base 16
    else byte          ^? base 16
  
  in case byte' of
    Nothing -> fail "Illegal hex number"
    Just b  -> b : byteArray rest

ceilTo :: (Integral a) => a -> a -> a
ceilTo n x = ceiling (fromIntegral x / fromIntegral n) * n

rawData :: Int -> Parser Data
rawData byteSize = try (asciiData byteSize) <|> try (hexData byteSize) <|> numData bitSize
  where
    bitSize = byteSize * 8

asciiData :: Int -> Parser Data
asciiData byteSize = do
  _           <- string "C"
  asciiString <- stringLiteral <?> "ascii"
  let
    nibs = byteSize * 2
    charLength = length asciiString 
    bytes = map (fromIntegral . fromEnum) asciiString ++ replicate (ceilTo byteSize charLength - charLength) 0
  pure $ Bytes bytes

hexData :: Int -> Parser Data
hexData byteSize = do
  _         <- string "X"
  hexString <- hexLiteral <?> "hex"
  let
    nibs = byteSize * 2
    nibLength = length hexString 
    bytes = byteArray $ hexString ++ replicate (ceilTo nibs nibLength - nibLength) '0'
  pure $ Bytes bytes

numData :: Int -> Parser Data
numData bitCount = Num <$> limitedInteger bitCount <?> "integer"