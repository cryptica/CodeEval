module Main where

import System.Environment (getArgs)

findCycle :: [Int] -> [Int]
findCycle [] = error "No cycle"
findCycle (x:xs) =
    let (toCycle, remainder) = break (== x) xs
    in  case remainder of
        [] -> findCycle xs
        _  -> x:toCycle

processLine :: String -> String
processLine = unwords . map show . findCycle. map read . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

