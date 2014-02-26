module Main where

import System.Environment(getArgs)
import qualified Data.Map as Map

hiddenMap :: Map.Map Char Char
hiddenMap = Map.fromList $ zip ['a'..'j'] ['0'..'9'] ++ zip ['0'..'9'] ['0'..'9']

showHidden :: Char -> String
showHidden c = case Map.lookup c hiddenMap of
    Just c' -> [c']
    Nothing -> ""

hiddenDigits :: String -> String
hiddenDigits = concatMap showHidden

processLine :: String -> String
processLine line = case hiddenDigits line of
    [] -> "NONE"
    s -> s

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

