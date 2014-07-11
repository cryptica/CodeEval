module Main where

import System.Environment(getArgs)
import Control.Monad

ageStatus :: Integer -> String
ageStatus x
        |  0 <= x && x <=   2 = "Home"
        |  3 <= x && x <=   4 = "Preschool"
        |  5 <= x && x <=  11 = "Elementary school"
        | 12 <= x && x <=  14 = "Middle school"
        | 15 <= x && x <=  18 = "High school"
        | 19 <= x && x <=  22 = "College"
        | 23 <= x && x <=  65 = "Work"
        | 66 <= x && x <= 100 = "Retirement"
        | otherwise          = "This program is for humans"

processLine :: String -> String
processLine = ageStatus . read

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

