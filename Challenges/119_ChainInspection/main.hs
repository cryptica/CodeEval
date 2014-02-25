module Main where

import System.Environment(getArgs)
import qualified Data.Map as Map

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

splitByPair :: Char -> String -> (String, String)
splitByPair c s =
    let [a,b] = splitBy c s
    in  (a,b)

intAddress :: String -> Int
intAddress "BEGIN" = 0
intAddress "END" = 1
intAddress s = read s

intPair :: (String, String) -> (Int, Int)
intPair (from, to) = (intAddress from, intAddress to)

followChain :: Int -> Map.Map Int Int -> Bool
followChain 1 chain = Map.size chain == 0
followChain from chain =
    case Map.lookup from chain of
        Just to -> followChain to (Map.delete from chain)
        Nothing -> False

processLine :: String -> String
processLine line =
    case followChain 0 $ Map.fromList $ map intPair $ map (splitByPair '-') $ splitBy ';' line of
        True -> "GOOD"
        False -> "BAD"


main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

