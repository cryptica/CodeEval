module Main where

import System.Environment(getArgs)

nonRepeatedChar :: [Char] -> Char
nonRepeatedChar = nonRepeatedChar' []
    where nonRepeatedChar' xs (c:cs) =
            if c `notElem` cs && c `notElem` xs then c
            else nonRepeatedChar' (c:xs) cs

processLine :: String -> String
processLine line = [ nonRepeatedChar line ]

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

