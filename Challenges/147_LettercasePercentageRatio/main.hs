module Main where

import System.Environment(getArgs)
import Control.Monad
import Data.List (partition)
import Data.Char (isLower)
import Text.Printf

lowerUpperPercentage :: String -> (Float, Float)
lowerUpperPercentage xs =
        let (lower,upper) = partition isLower xs
            ll = fromIntegral $ length lower
            lu = fromIntegral $ length upper
            n  = ll + lu
        in  (100 * ll / n, 100 * lu / n)

processLine :: String -> IO ()
processLine line =
        uncurry (printf "lowercase: %.2f uppercase: %.2f\n")
                (lowerUpperPercentage line)

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ processLine

