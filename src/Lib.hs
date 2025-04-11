module Lib (frontEnd) where

import System.Environment (getArgs)
import Parsers (interpreter)
import Text.Megaparsec


frontEnd :: IO ()
frontEnd = do
    args <- getArgs
    case args of
        [file, input] -> do
            putStrLn (file ++ " " ++ input)
            contents <- readFile file
            let result = parse (interpreter input) file contents
            case result of
                Left err -> putStrLn $ errorBundlePretty err
                Right val -> putStrLn val
        _ -> putStrLn "Usage: stack run -- path/to/file.to input"
