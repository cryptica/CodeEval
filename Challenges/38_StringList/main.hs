module Main where

import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Set as S

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

stringList :: Int -> [Char] -> [String]
stringList n c =
    if n == 0 then [""]
    else do
        c1 <- c
        s2 <- stringList (n - 1) c
        return (c1 : s2)

processLine :: String -> String
processLine line =
    let [n,s] = splitBy ',' line
    in  intercalate "," $ stringList (read n) (S.toAscList $ S.fromList s)

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input
