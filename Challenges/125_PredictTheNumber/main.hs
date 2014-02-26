module Main where

import System.Environment(getArgs)

-- Integer log base
imLog :: Int -> Int -> Int
imLog b x =
    if x < b then
      0
    else
      let
        l = 2 * imLog (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
        doDiv (x`div`(b^l)) l

replace :: Int -> Int
replace 0 = 1
replace 1 = 2
replace 2 = 0

numberN :: Int -> Int
numberN 0 = 0
numberN n = replace $ numberN (n - 2^(imLog 2 n))

processLine :: String -> String
processLine = show . numberN . read

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

