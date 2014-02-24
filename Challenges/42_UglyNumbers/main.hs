module Main where

import System.Environment (getArgs)

type Expression = [String]

isUglyNumber :: Int -> Bool
isUglyNumber n =
    n `divBy` 2 || n `divBy` 3 || n `divBy` 5 || n `divBy` 7
    where x `divBy` y = x `mod` y == 0

evaluateExpression' :: Int -> Expression -> Int
evaluateExpression' acc [] = acc
evaluateExpression' acc ("+":x:xs) = evaluateExpression' (acc + read x) xs
evaluateExpression' acc ("-":x:xs) = evaluateExpression' (acc - read x) xs
evaluateExpression' _ _ = error "Invalid Expression"

evaluateExpression :: Expression -> Int
evaluateExpression [] = error "Invalid Expression"
evaluateExpression (x:xs) = evaluateExpression' (read x) xs

addExpressions :: Char -> [Expression] -> [Expression]
addExpressions _ [] = []
addExpressions _ ([]:_) = error "Invalid Expression"
addExpressions c (e@(cs:es):exps) =
    ([c]:"+":e) : ([c]:"-":e) : ((c:cs):es) : (addExpressions c exps)

makeExpressions :: String -> [Expression]
makeExpressions [] = []
makeExpressions [c] = [[[c]]]
makeExpressions (c:cs) =
    let subExpressions = makeExpressions cs
    in  addExpressions c subExpressions

simplifyDigits :: String -> (Int, String)
simplifyDigits s =
    let (zeros, remainder) = span (== '0') (init s)
    in  (length zeros, remainder ++ [last s])

countUglyExpressions :: String -> Int
countUglyExpressions s =
    let (factor, simpString) = simplifyDigits s
        exps = makeExpressions simpString
        evaluatedExps = map evaluateExpression exps
        uglyExps = length $ filter isUglyNumber evaluatedExps
    in  3^factor * uglyExps

processLine :: String -> String
processLine = show . countUglyExpressions

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

