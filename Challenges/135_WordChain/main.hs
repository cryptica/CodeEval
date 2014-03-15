module Main where

import System.Environment(getArgs)
import Data.List (delete)
import qualified Data.Map as M

type ChainMap = M.Map Char [Char]

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

buildChainMap :: [String] -> ChainMap
buildChainMap [] = M.empty
buildChainMap (x:xs) = M.insertWith (++) (head x) [last x] $ buildChainMap xs

followChain :: ChainMap -> Char -> Int 
followChain chainMap x =
    let followChain' y = followChain (M.adjust (delete y) x chainMap) y
        maxChains = map followChain' (M.findWithDefault [] x chainMap)
    in  if null maxChains then 0 else 1 + maximum maxChains

processLine' :: String -> String
processLine' line =
    let chainMap = buildChainMap (splitBy ',' line)
        allChains = map (followChain chainMap) (M.keys chainMap)
        maxChain = maximum allChains
    in  if maxChain == 1 then "None"
        else show maxChain

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine' $ lines input

