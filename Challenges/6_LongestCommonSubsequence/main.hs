module Main where

import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)

longestCommonSubsequence :: String -> String -> String
longestCommonSubsequence [] _ = []
longestCommonSubsequence _ [] = []
longestCommonSubsequence (x:xs) (y:ys) =
    if x == y then x : (longestCommonSubsequence xs ys)
    else
        let lcs1 = longestCommonSubsequence (x:xs) ys
            lcs2 = longestCommonSubsequence xs (y:ys)
        in  maximumBy (comparing length) [lcs1,lcs2]

processLine :: String -> String
processLine line =
    let (s1,_:s2) = break (== ';') line
    in  longestCommonSubsequence s1 s2

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

