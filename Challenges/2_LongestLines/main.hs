import System.Environment
import Data.List
import Data.Ord

nLongestLines :: Int -> [String] -> [String]
nLongestLines n inputLines =
    take n $ sortBy (flip $ comparing length) inputLines

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    let ns : inputLines = lines input
    mapM_ putStrLn $ nLongestLines (read ns) inputLines

