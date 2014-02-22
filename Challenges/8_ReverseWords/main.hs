import System.Environment

reverseLine :: String -> String
reverseLine =
    unwords . reverse . words

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map reverseLine $ lines input

