module Main where

import System.Environment(getArgs)
import Control.Monad
import Data.Char  (digitToInt)

type Aromatic = [(Int,Int)]

evalAromatic :: Aromatic -> Int
evalAromatic [] = 0
evalAromatic ((v1,b1):xs) =
        let  sign = case xs of
                       ((_,b2):_) | b1 < b2 -> -1
                       _ -> 1
        in  sign*v1*b1 + evalAromatic xs

parseAromatic :: String -> Aromatic
parseAromatic [] = []
parseAromatic (a:r:xs) =
        let v = digitToInt a
            b = case r of
                     'I' -> 1
                     'V' -> 5
                     'X' -> 10
                     'L' -> 50
                     'C' -> 100
                     'D' -> 500
                     'M' -> 1000
        in (v,b) : parseAromatic xs

processLine :: String -> String
processLine = show . evalAromatic . parseAromatic

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

