module Main where

import System.Environment(getArgs)

primes :: [Int]
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

countPrimes :: Int -> Int -> Int
countPrimes lower upper =
    length $ takeWhile (<= upper) $ dropWhile (< lower) $ primes

processLine :: String -> String
processLine line =
    let (lower,upper) = read ("(" ++ line ++ ")")
    in  show $ countPrimes lower upper

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

