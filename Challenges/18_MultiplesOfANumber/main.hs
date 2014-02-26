module Main where

import System.Environment(getArgs)

smallestMultiple :: Int -> Int -> Int
smallestMultiple x n = n * ceiling (fromIntegral x / fromIntegral n :: Float)

processLine :: String -> String
processLine line = 
    let (x,n) = read ("(" ++ line ++ ")")
    in  show $ smallestMultiple x n

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

