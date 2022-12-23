module Main where

import Parsers.Parser
import Assembler.Assembler

import Text.Megaparsec

main :: IO ()
main = do
    input <- readFile "echo.asm"
    let parsed = parse ass "" input
    case parsed of
        Left e -> putStrLn (errorBundlePretty e)
        Right m -> do
            mapM_ print m
            let obj = fromParsed m
            putStrLn (toRaw obj)