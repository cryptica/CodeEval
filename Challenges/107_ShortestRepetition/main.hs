module Main where

import System.Environment(getArgs)

shortestRepetition :: String -> Int
shortestRepetition s = period 0 0 1 s s
    where period strPos repPos repLen _ [] = if repPos == repLen then repPos else strPos
          period strPos repPos repLen (r:rs) word@(w:ws)
            | repPos == repLen = period  strPos     0         repLen    s  word
            | r == w           = period (strPos+1) (repPos+1) repLen    rs ws
            | repPos == 0      = period (strPos+1)  0        (strPos+1) s  ws
            | otherwise        = period  strPos     0         strPos    s  word

processLine :: String -> String
processLine = show . shortestRepetition

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

