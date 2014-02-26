module Main where

import System.Environment(getArgs)

alternate :: [a] -> [a]
alternate [] = []
alternate [x] = [x]
alternate (x:_:xs) = x : alternate xs

alternateStack :: [a] -> [a]
alternateStack = alternate . reverse

processLine :: String -> String
processLine = unwords . alternateStack . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

