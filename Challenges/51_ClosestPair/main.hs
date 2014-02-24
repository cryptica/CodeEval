module Main where

import System.Environment (getArgs)
import Text.Printf

type Point = (Float, Float)

stringToPoint :: String -> Point
stringToPoint s =
    let [x,y] = map read $ words s
    in  (x,y)

distance :: (Point,Point) -> Float
distance ((x1,y1),(x2,y2)) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

closestPairDistance :: [Point] -> Float
closestPairDistance ps =
    let pairs = [(p1,p2) | p1 <- ps, p2 <- ps, p1 /= p2]
    in  minimum $ map distance pairs

processInput :: [String] -> [Float]
processInput [] = []
processInput (x:xs) =
    let n = read x
        (pointStrings, xs') = splitAt n xs
        remaining = processInput xs'
    in
        if n == 0 then remaining
        else
            let points = map stringToPoint pointStrings
                closestDistance = closestPairDistance points
            in  closestDistance : remaining

printResult :: Float -> IO ()
printResult f =
    if f > 10000.0 then putStrLn "INFINITY"
    else printf "%.4f\n" f

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ printResult $ processInput $ lines input

