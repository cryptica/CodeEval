module Main where

import System.Environment(getArgs)
import Data.Char (toLower, toUpper, isLower, isUpper)

swapCase :: Char -> Char
swapCase c
    | isLower c = toUpper c
    | isUpper c = toLower c
    | otherwise = c

processLine :: String -> String
processLine = map swapCase

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

