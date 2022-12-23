module Main where

import Parsers.Parser
import Assembler.Assembler

import Text.Megaparsec
import System.Environment

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
    (file : _) <- getArgs
    let withoutExtension = takeWhile (/= '.')
    input <- readFile file
    case run ass input of
        Left e -> putStrLn $ errorBundlePretty e
        Right m -> do
            mapM_ print m
            let obj = fromParsed m
            writeFile (withoutExtension file ++ ".obj") (toRaw obj)