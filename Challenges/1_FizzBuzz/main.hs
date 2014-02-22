import System.Environment

fizzBuzz :: Int -> Int -> Int -> String
fizzBuzz a b x =
    let diva = x `mod` a == 0
        divb = x `mod` b == 0
    in
    if diva && divb then "FB"
    else if diva then "F"
    else if divb then "B"
    else show x

fizzBuzzList :: Int -> Int -> [Int] -> String
fizzBuzzList a b = unwords . map (fizzBuzz a b)

fizzBuzzLine :: String -> String
fizzBuzzLine line =
    let [a, b, n] = map read $ words line
    in fizzBuzzList a b [1 .. n]

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map fizzBuzzLine $ lines input

