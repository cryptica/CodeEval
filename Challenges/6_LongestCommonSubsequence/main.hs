module Main where

import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

getLcs :: Map.Map (String,String) String -> String -> String -> (String, Map.Map (String,String) String)
getLcs lcsMap s1 s2 = case Map.lookup (s1,s2) lcsMap of
    Just l -> (l, lcsMap)
    Nothing ->
        let (lcs', lcsMap') = lcs lcsMap s1 s2
        in  (lcs', Map.insert (s1,s2) lcs' lcsMap')

lcs :: Map.Map (String,String) String -> String -> String -> (String, Map.Map (String,String) String)
lcs lcsMap [] _ = ([], lcsMap)
lcs lcsMap _ [] = ([], lcsMap)
lcs lcsMap (x:xs) (y:ys) =
    if x == y then
        let (lcs', lcsMap') = getLcs lcsMap xs ys
        in  (x : lcs', lcsMap')
    else
        let (lcs1, lcsMap') = getLcs lcsMap (x:xs) ys
            (lcs2, lcsMap'') = getLcs lcsMap' xs (y:ys)
        in  (maximumBy (comparing length) [lcs1,lcs2], lcsMap'')

processLine :: String -> String
processLine line =
    let (s1,_:s2) = break (== ';') line
    in  fst $ lcs Map.empty s1 s2

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

