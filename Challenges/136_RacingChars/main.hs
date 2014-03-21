module Main where

import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

replaceChar :: String -> Int -> Char -> String
replaceChar s i c =
    let (before,_:after) = splitAt i s
    in  before ++ [c] ++ after

race' :: [String] -> Int -> Maybe (Int, [String])
race' [] _ = Just (0, [])
race' (x:xs) pos = max (goTo '_' 0) (goTo 'C' 1)
    where goTo c inc = do
            pos' <- elemIndex c x
            if pos < 0 || abs (pos' - pos) <= 1 then do
                let sym = if pos < 0 || pos' == pos then '|'
                          else if pos' < pos then '/'
                          else '\\'
                (score, track) <- race' xs pos'
                return (score + inc, (replaceChar x pos' sym):track)
            else Nothing

race :: [String] -> [String]
race xs = snd $ fromJust $ race' xs (-1)

processInput :: String -> String
processInput = unlines . race . lines

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    putStr $ processInput input

