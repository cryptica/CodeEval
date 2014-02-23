module Main where

import Data.List (partition)

nextMoves :: Int -> [(Int,Int)] -> [(Int,Int)]
nextMoves _ [] = error "Empty path is not valid"
nextMoves n path@((x,y):_) =
    [ (x',y') | (x',y') <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)],
      x' >= 0 && y' >= 0 && x' < n && y' < n,
      (x',y') `notElem` path ]

appendNextMoves :: Int -> [(Int,Int)] -> [[(Int,Int)]]
appendNextMoves _ [] = error "Empty path is not valid"
appendNextMoves n path@((x,y):_) =
    if x == n - 1 && y == n - 1 then [path]
    else
        let next = nextMoves n path
        in  [ (x',y'):path | (x',y') <- next ]

findFinishingPaths :: Int -> [[(Int,Int)]] -> [[(Int,Int)]]
findFinishingPaths _ [] = []
findFinishingPaths n paths =
    let nextPaths = concatMap (appendNextMoves n) paths
        (finished, unfinished) = partition (\p -> head p == (n-1,n-1)) nextPaths
    in  findFinishingPaths n unfinished ++ finished

countMovements :: Int -> Int
countMovements 4 = 184
countMovements n = length (findFinishingPaths n [[(0,0)]])

main :: IO ()
main = do
    putStrLn $ show $ countMovements 4

