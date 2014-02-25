module Main where

import System.Environment(getArgs)

combineRows :: [Int] -> [Int] -> [Int]
combineRows (x:xs) (y:ys) = scanl minPath (x + y) (zip xs ys)
    where minPath top (left, dest) = dest + min top left

minimumPathSum :: [[Int]] -> Int
minimumPathSum (x:xs) = last $ foldl combineRows (scanl1 (+) x) xs

processInput :: [String] -> [String]
processInput [] = []
processInput (x:xs) =
    let (matrixLines, remaining) = splitAt (read x) xs
        matrix = map (\row -> read ("[" ++ row ++ "]")) matrixLines
    in  (show $ minimumPathSum matrix) : (processInput remaining)

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ processInput $ lines input

