module Main where

import System.Environment(getArgs)
import Control.Monad
import Data.Char  (digitToInt)
import Numeric    (readInt)

zeroSeqToBinString :: [String] -> String
zeroSeqToBinString [] = ""
zeroSeqToBinString ("0":x:xs) = x ++ zeroSeqToBinString xs
zeroSeqToBinString ("00":x:xs) = map (\'0' -> '1') x ++ zeroSeqToBinString xs

binStringToInt :: String -> Int
binStringToInt = fst . head . readInt 2 (`elem` "01") digitToInt

zeroSeqToInt :: [String] -> Int
zeroSeqToInt = binStringToInt . zeroSeqToBinString

processLine :: String -> String
processLine = show . zeroSeqToInt . words

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

