module Main where

import System.Environment(getArgs)
import Data.List (partition, intercalate)
import Data.Char (isDigit)

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

isWord :: String -> Bool
isWord = any (not . isDigit)

separateWords :: [String] -> ([String],[String])
separateWords = partition isWord

processLine :: String -> String
processLine line =
    let wordDigitList = splitBy ',' line
        listToString = intercalate ","
    in  case separateWords wordDigitList of
        ([], ds) -> listToString ds
        (ws, []) -> listToString ws
        (ws, ds) -> listToString ws ++ "|" ++ listToString ds

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

