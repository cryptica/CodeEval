module Main where

import System.Environment(getArgs)
import Numeric (readFloat)
import Data.List (maximumBy)
import Data.Function (on)

type Thing = (Int, Rational, Int)
type Package = ([Int], Rational, Int)

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

readRational :: String -> Rational
readRational = fst . head . readFloat

addThing :: Thing -> Package -> Package
addThing (index, weight, cost) (indices, totalWeight, price) =
    (index:indices, weight + totalWeight, cost + price)

packThings :: Rational -> [Thing] -> [Package]
packThings _ [] = [([], 0, 0)]
packThings remWeight (x@(_,weight,_):xs) =
    (if weight <= remWeight then
        let packages = packThings (remWeight - weight) xs
        in map (addThing x) packages
    else []) ++ packThings remWeight xs

bestPackage :: [Package] -> Package
bestPackage = maximumBy (compare `on` (\(_, w, p) -> (p, -w)))

makeThing :: String -> Thing
makeThing s =
    let [s1,s2,s3] = splitBy ',' $ tail $ init s
    in  (read s1, readRational s2, read $ tail s3)

processLine :: String -> String
processLine line =
    let (maxWeightS:_:thingsS) = words line
        maxWeight = readRational maxWeightS
        things = map makeThing thingsS
        packages = packThings maxWeight things
    in  case bestPackage packages of
            ([], _, _) -> "-"
            (indices, _, _) -> tail $ init $ show indices

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

