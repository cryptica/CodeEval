module Main where

import System.Environment(getArgs)
import Data.Char (ord)

sumOfDigits :: String -> Int
sumOfDigits = sum . map (\c -> ord c - ord '0')

processLine :: String -> String
processLine = show . sumOfDigits

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

