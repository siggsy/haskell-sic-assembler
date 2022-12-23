module Main where

import Parsers.Parser
import Assembler.Assembler

import Text.Megaparsec

initialState :: String -> Int -> s -> State s e
initialState src tabWidth s = State
  { stateInput  = s
  , stateOffset = 0
  , statePosState = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos src
    , pstateTabWidth = mkPos tabWidth
    , pstateLinePrefix = ""
    }
  , stateParseErrors = []
  }

run ::
  Parsec e s a ->
  s ->
  Either (ParseErrorBundle s e) a
run p input = snd $ runParser' p $ initialState "" 4 input

main :: IO ()
main = do
    input <- readFile "echo.asm"
    let parsed = run ass input
    case parsed of
        Left e -> putStrLn (errorBundlePretty e)
        Right m -> do
            mapM_ print m
            let obj = fromParsed m
            putStrLn (toRaw obj)