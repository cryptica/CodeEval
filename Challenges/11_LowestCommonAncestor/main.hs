module Main where

import System.Environment(getArgs)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

searchTree :: Tree Int
searchTree =
    Node 30
        (Node 8
            (Node 3 EmptyTree EmptyTree)
            (Node 20
                (Node 10 EmptyTree EmptyTree)
                (Node 29 EmptyTree EmptyTree)
            )
        )
        (Node 52 EmptyTree EmptyTree)

lca :: (Ord a) => a -> a -> Tree a -> a
lca _ _ EmptyTree = error "Node not in tree"
lca n1 n2 (Node x left right)
    | n1 == x = n1
    | n2 == x = n2
    | n1 < x && n2 > x = x
    | n1 > x && n2 < x = x
    | n1 < x && n2 < x = lca n1 n2 left
    | n1 > x && n2 > x = lca n1 n2 right

processLine :: String -> String
processLine line =
    let [n1,n2] = map read $ words line
    in  show $ lca n1 n2 searchTree

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

