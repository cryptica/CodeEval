module Main where

import System.Environment (getArgs)
import Data.List (intercalate, permutations, sort)

processLine :: String -> String
processLine = intercalate "," . sort . permutations

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

