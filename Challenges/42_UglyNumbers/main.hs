module Main where

import System.Environment (getArgs)

isUglyNumber :: Int -> Bool
isUglyNumber n =
    n `divBy` 2 || n `divBy` 3 || n `divBy` 5 || n `divBy` 7
    where x `divBy` y = x `mod` y == 0

evaluateExpression' :: Int -> [String] -> Int
evaluateExpression' acc [] = acc
evaluateExpression' acc ("+":x:xs) = evaluateExpression' (acc + read x) xs
evaluateExpression' acc ("-":x:xs) = evaluateExpression' (acc - read x) xs
evaluateExpression' _ _ = error "Invalid Expression"

evaluateExpression :: [String] -> Int
evaluateExpression [] = error "Invalid Expression"
evaluateExpression (x:xs) = evaluateExpression' (read x) xs

makeExpressions :: String -> [[String]]
makeExpressions [] = []
makeExpressions [c] = [[[c]]]
makeExpressions (c1:c2:cs) =
    let subExpressions = makeExpressions (c2:cs)
    in  [ [c1]:"+":cs' | cs' <- subExpressions ] ++
        [ [c1]:"-":cs' | cs' <- subExpressions ] ++
        [ (c1:c):cs' | (c:cs') <- subExpressions ]

countUglyExpressions :: String -> Int
countUglyExpressions =
    length . filter isUglyNumber . map evaluateExpression . makeExpressions

processLine :: String -> String
processLine = show . countUglyExpressions

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

