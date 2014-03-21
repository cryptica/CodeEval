module Main where

import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid

replaceChar :: String -> Int -> Char -> String
replaceChar s i c =
    let (before,_:after) = splitAt i s
    in  before ++ [c] ++ after

race' :: [String] -> Int -> [String]
race' [] _ = []
race' (x:xs) pos =
    let pos' = fromJust $ getFirst $ mconcat $ map First $ map (`elemIndex` x) "C_"
        sym = if pos < 0 || pos' == pos then '|' else if pos' < pos then '/' else '\\'
    in  (replaceChar x pos' sym):(race' xs pos')

race :: [String] -> [String]
race xs = race' xs (-1)

processInput :: String -> String
processInput = unlines . race . lines

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    putStr $ processInput input

