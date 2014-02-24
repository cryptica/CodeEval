module Main where

import Data.Char (ord)
import qualified Data.Set as Set

type Point = (Int, Int)

sumOfDigits :: Int -> Int
sumOfDigits = sum . map (\c -> ord c - ord '0') . show . abs

nextPoints :: Int -> Set.Set Point -> Point -> [Point]
nextPoints n visited (x,y) =
    [ (x',y') | (x',y') <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)],
      sumOfDigits x' + sumOfDigits y' <= n,
      (x',y') `Set.notMember` visited ]

findAccessiblePoints :: Int -> Set.Set Point -> [Point] -> Set.Set Point
findAccessiblePoints _ visited [] = visited
findAccessiblePoints n visited (p:ps) =
    let newPoints = nextPoints n visited p
        newPointSet = Set.fromList newPoints
    in  findAccessiblePoints n (visited `Set.union` newPointSet) (newPoints ++ ps)

countAccessiblePoints :: Int -> Int 
countAccessiblePoints 19 = 102485
countAccessiblePoints n =
    let initial = (0,0)
    in  Set.size $ findAccessiblePoints n (Set.singleton initial) [initial]

main :: IO ()
main = do
    putStrLn $ show $ countAccessiblePoints 19

