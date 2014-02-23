module Main where

import System.Environment (getArgs)

duplicateEntry :: Int -> [Int] -> Int
duplicateEntry n a =
    let arraySum = sum a
        properSum = (n-2)*(n-1) `div` 2
    in arraySum - properSum

processLine :: String -> String
processLine line =
    let (ns, _:na) = break (== ';') line
        n = read ns
        a = read ("[" ++ na ++ "]")
    in  show $ duplicateEntry n a

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

