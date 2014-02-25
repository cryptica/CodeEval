module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

insertUpdateLookup :: (Ord k) => (a -> a) -> k -> a -> Map.Map k a -> (a, Map.Map k a)
insertUpdateLookup f key val m =
    case (Map.insertLookupWithKey (\_ _ oldVal -> f oldVal) key val m) of
        (Just oldVal, m') -> (f oldVal, m')
        (Nothing, m') -> (val, m')

majorElement' :: (Ord k) => Int -> Map.Map k Int -> [k] -> Maybe k
majorElement' _ _ [] = Nothing
majorElement' target m (l:ls) =
    let (val, m') = insertUpdateLookup (+1) l 1 m
    in  if val > target then Just l
        else majorElement' target m' ls

majorElement :: (Ord k) => [k] -> Maybe k
majorElement ls = majorElement' (length ls `div` 2) Map.empty ls

processLine :: String -> String
processLine line =
    let elements = splitBy ',' line
        major = majorElement elements
    in  case major of
            Nothing -> "None"
            Just m -> m

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

