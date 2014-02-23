module Main where

import System.Environment (getArgs)
import Data.List (permutations)
import Data.Char (toLower)

isOrderedSquare :: [(Int,Int)] -> Bool
isOrderedSquare [(x1,y1),(x2,y2),(x3,y3),(x4,y4)] =
    let l1 = (x1 - x2)^2 + (y1 - y2)^2
        l2 = (x2 - x3)^2 + (y2 - y3)^2
        l3 = (x3 - x4)^2 + (y3 - y4)^2
        l4 = (x4 - x1)^2 + (y4 - y1)^2
        l5 = (x4 - x2)^2 + (y4 - y2)^2
        l6 = (x3 - x1)^2 + (y3 - y1)^2
    in  l1 == l2 && l2 == l3 && l3 == l4 && l5 == l6 && l1 > 0
isOrderedSquare _ = False

isSquare :: [(Int,Int)] -> Bool
isSquare = any isOrderedSquare . permutations

processLine :: String -> String
processLine line = map toLower . show . isSquare $ read ("[" ++ line ++ "]")

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

