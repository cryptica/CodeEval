module Main where

import System.Environment(getArgs)

parseLine :: String -> [String]
parseLine [] = [""]
parseLine (':':xs@('(':xss)) = parseLine xss ++ parseLine xs
parseLine (':':xs@(')':xss)) = parseLine xss ++ parseLine xs
parseLine ('(':xs) =
    let xsParses = parseLine xs
        matchParentheses (')':xs') = parseLine xs'
        matchParentheses _ = []
    in  concatMap matchParentheses xsParses
parseLine xs@(')':_) = [xs]
parseLine (_:xs) = parseLine xs

processLine :: String -> String
processLine line =
    if any (=="") $ parseLine line then "YES" else "NO"

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

