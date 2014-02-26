module Main where

import System.Environment(getArgs)
import Data.List (sort)

findUniquePos :: [(Int, Int)] -> Int
findUniquePos [] = 0
findUniquePos [(_, p)] = p
findUniquePos ((x, p):(y, _):xs) =
    if x /= y then p
    else findUniquePos (dropWhile ((== x) . fst) xs)

determineWinner :: [Int] -> Int
determineWinner numbers = findUniquePos $ sort $ zip numbers [1..]

processLine :: String -> String
processLine = show . determineWinner . map read . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

