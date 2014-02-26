module Main where

import System.Environment(getArgs)

isEven :: Int -> Bool
isEven i = i `mod` 2 == 0

processLine :: String -> String
processLine line = case isEven (read line) of
    True -> "1"
    False -> "0"

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

