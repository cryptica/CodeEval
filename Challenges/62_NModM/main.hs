module Main where

import System.Environment(getArgs)

mymod :: Int -> Int -> Int
n `mymod` m = n - n `div` m * m

processLine :: String -> String
processLine line =
    let (n,m) = read ("(" ++ line ++ ")")
    in  show $ n `mymod` m

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

