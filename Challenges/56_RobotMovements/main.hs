module Main where

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

appendAllNextMovements :: Int -> [[(Int,Int)]] -> [[(Int,Int)]]
appendAllNextMovements n paths = concatMap (appendNextMoves n) paths

countMovements :: Int -> Int
countMovements 4 = 184
countMovements n = length ((iterate (appendAllNextMovements n) [[(0,0)]]) !! (n*n))

main :: IO ()
main = do
    putStrLn $ show $ countMovements 4

