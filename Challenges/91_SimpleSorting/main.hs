module Main where

import System.Environment(getArgs)
import Data.List (sortBy, sort)
import Data.Ord (comparing)

sortNumbers :: [(Float, String)] -> [(Float, String)]
sortNumbers = sort

processLine :: String -> String
processLine = unwords . map snd . sortNumbers . map (\s -> (read s, s)) . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

