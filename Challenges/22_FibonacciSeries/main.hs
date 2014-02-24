{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)

-- tree structure for memoization
data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

nats :: Tree Int
nats = go 0 1
    where
        go !n !s = Tree (go l s') n (go r s')
            where
                l = n + s
                r = l + s
                s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0..]

-- define fibonacci function
fib :: (Int -> Integer) -> Int -> Integer
fib _ 0 = 0
fib _ 1 = 1
fib mf n = mf (n - 1) + mf (n - 2)

fib_tree :: Tree Integer
fib_tree = fmap (fib fastest_fib) nats

fastest_fib :: Int -> Integer
fastest_fib = index fib_tree

-- solve problem
processLine :: String -> String
processLine = show . fastest_fib . read

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

