module Main where

import System.Environment(getArgs)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

digitMap :: Map.Map T.Text Char
digitMap =
    Map.fromList [
        (T.pack "zero", '0'),
        (T.pack "one", '1'),
        (T.pack "two", '2'),
        (T.pack "three", '3'),
        (T.pack "four", '4'),
        (T.pack "five", '5'),
        (T.pack "six", '6'),
        (T.pack "seven", '7'),
        (T.pack "eight", '8'),
        (T.pack "nine", '9')
    ]

wordToDigit :: T.Text -> Char
wordToDigit text = fromJust $ Map.lookup text digitMap

processLine :: T.Text -> String
processLine = map wordToDigit . T.split (==';')

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- TIO.readFile inputFile
    mapM_ putStrLn $ map processLine $ T.lines input

