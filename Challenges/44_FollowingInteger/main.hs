module Main where

import System.Environment (getArgs)
import Data.List (sort, permutations)

followingInteger :: Int -> Int
followingInteger n =
    head $ sort $ filter (> n) $ map read $ permutations $ ("0" ++ show n)

processLine :: String -> String
processLine = show . followingInteger . read

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

