{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Loader where
import Machine hiding (get, t)
import Data.List.Split
import Data.List
import Data.Semigroup
import Numeric
import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Data.Ix
import Control.Lens

data Record = 
    H String Address Int |
    E Address |
    T Address Int [SicByte] |
    D String |
    Unknown deriving Show

hexDigit :: ReadP Char
hexDigit = satisfy (\char -> inRange ('0', '9') char || inRange ('A', 'F') char)

hexNum :: Int -> ReadP Int
hexNum digits = do 
    hex <- count digits hexDigit
    return $ fromIntegral . fst . head . readHex $ hex

nib :: ReadP SicByte
nib = fromIntegral <$> hexNum 2

nibs :: (Integral a) => a -> ReadP [SicByte]
nibs n = count (fromIntegral n) nib

h :: ReadP Record
h = do
    char 'H'
    name <- count 6 get
    addr <- hexNum 6
    len  <- hexNum 6
    skipSpaces
    return $ H (takeWhile (/=' ') name) (fromIntegral addr) len

e :: ReadP Record
e = do 
    char 'E'
    addr <- hexNum 6
    skipSpaces
    return $ E (fromIntegral addr)

t :: ReadP Record
t = do
    char 'T'
    addr    <- hexNum 6
    len     <- nib
    code    <- nibs len
    skipSpaces
    return $ T (fromIntegral addr) (fromIntegral len) code

obj :: ReadP [Record]
obj = do 
    records <- many1 (h +++ e +++ t)
    eof
    return records

readObj :: String -> [Record]
readObj = fst . head . readP_to_S obj

objStruct :: [Record] -> (Maybe Record, [Record], Maybe Record)
objStruct records = let
    munchRecord (h, ts, e) r = case r of
        T {} -> (h, r:ts, e)
        E {} -> (h, ts, Just r)
        H {} -> (Just r, ts, e)
    in foldl' munchRecord (Nothing, [], Nothing) records

code :: (Maybe Record, [Record], Maybe Record) -> [(Address, SicByte)]
code (_, ts, _) = 
    [ (byteAddr, byte)
    | T addr len bytes <- ts
    , (byteAddr, byte) <- zip [addr .. addr + fromIntegral len] bytes 
    ]

load :: String -> Update
load obj = let 
    records = readObj obj
    struct = objStruct records
    in (foldl1' (>>) $ map loadAt (code struct))
    where
        loadAt :: (Address, SicByte) -> Update
        loadAt (addr, b) = byte addr .= b
