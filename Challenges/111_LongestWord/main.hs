module Main where

import System.Environment(getArgs)
import Control.Monad (liftM)
import Data.List (minimumBy)
import Data.Ord (comparing)

processLine :: String -> String
processLine = minimumBy (flip $ comparing length) . words

main :: IO ()
main = liftM head getArgs >>= readFile >>= mapM_ (putStrLn . processLine) . lines

