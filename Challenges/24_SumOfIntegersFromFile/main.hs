module Main where

import System.Environment(getArgs)

sumOfIntegers :: [Int] -> Int
sumOfIntegers = sum

processInput :: [String] -> String
processInput = show . sumOfIntegers . map read

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    putStrLn $ processInput $ lines input

