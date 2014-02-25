module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

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

processLine :: T.Text -> T.Text
processLine line =
    let elements = T.split (==',') line
        major = majorElement elements
    in  case major of
            Nothing -> T.pack "None"
            Just m -> m

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- TIO.readFile inputFile
    mapM_ TIO.putStrLn $ map processLine $ T.lines input

