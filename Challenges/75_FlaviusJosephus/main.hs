module Main where

import System.Environment(getArgs)

execute :: Int -> Int -> [Int]
execute n m = execute' 1 [0..n-1] []
    where execute' _ [] [] = []
          execute' i [] ys = execute' i (reverse ys) []
          execute' i (x:xs) ys
            | i == m = x : execute' 1 xs ys
            | otherwise = execute' (i+1) xs (x:ys)

processLine :: String -> String
processLine line =
    let (n,m) = read ("(" ++ line ++ ")")
    in  unwords . map show $ execute n m

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

