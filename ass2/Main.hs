import Commands
import Machine
import Loader
import System.Environment

main :: IO ()
main = do
    [objFile]   <- getArgs
    obj         <- readFile objFile
    
    let m       = prog @ initialize
        prog    = load obj

    start m
    return ();

start :: Machine -> IO Machine
start state = do
    print state
    putStrLn ""
    putStrLn $ showMemory 0x00 0x50 (state)
    c <- getLine
    if c == "n"
        then do
            start (step state)
        else do 
            putStrLn "Unknown Command" 
            return state


step :: Machine -> Machine
step state = 
    let command = parseCommand state
    in command @ state

run :: Int -> Machine -> Machine
run n state =
    foldr (\_ state' -> step state') state [0..n]