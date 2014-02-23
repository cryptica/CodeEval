module Main where

import System.Environment (getArgs)

data Operator = Plus | Minus | Times | Divide deriving (Show)
data Token = Op Operator | Num Float deriving (Show)

evaluate :: [Token] -> (Float, [Token])
evaluate [] = error "Value undefined"
evaluate (Num n : xs) = (n, xs)
evaluate (Op op : xs) = 
    let f = operatorToFunction op
        (n1, xs' ) = evaluate xs
        (n2, xs'') = evaluate xs'
    in  (n1 `f` n2, xs'')

operatorToFunction :: Operator -> (Float -> Float -> Float)
operatorToFunction Plus   = (+)
operatorToFunction Minus  = (-)
operatorToFunction Times  = (*)
operatorToFunction Divide = (/)

stringToToken :: String -> Token
stringToToken "+" = Op Plus
stringToToken "-" = Op Minus
stringToToken "*" = Op Times
stringToToken "/" = Op Divide
stringToToken n = Num (read n)

processLine :: String -> String
processLine = show . (truncate :: Float -> Integer) . fst . evaluate . map stringToToken . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

