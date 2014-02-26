module Main where

import System.Environment(getArgs)
import Data.Char (toLower, isAlpha)
import Data.List (sort, group)

beautyNumber :: String -> Int
beautyNumber s =
    let letters = map toLower $ filter isAlpha s
        counts = reverse $ sort $ map length $ group $ sort letters
    in  sum $ map (uncurry (*)) $ zip counts [26,25..1]

processLine :: String -> String
processLine = show . beautyNumber

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

