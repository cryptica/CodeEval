module Main where

import System.Environment(getArgs)
import Control.Monad

freeSpaces :: Int -> Int -> [Int] -> [Int]
freeSpaces l p [] = [l-p]
freeSpaces l p (x:xs) = (x-p) : freeSpaces l x xs

placeBats :: Int -> Int -> Int -> Int -> Int
placeBats ld d rd s = if s >= ld + rd then ((s - ld - rd) `div` d) + 1 else 0

determineMaxBats :: Int -> Int -> [Int] -> Int
determineMaxBats l d xs =
        case freeSpaces l 0 xs of
            [y] -> placeBats 6 d 6 y
            (y:ys) -> placeBats 6 d d y +
                     sum (map (placeBats d d d) (init ys)) +
                     placeBats d d 6 (last ys)

processLine :: String -> String
processLine line =
        let (l:d:_:xs) = map read $ words line
        in  show $ determineMaxBats l d xs

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

