module Main where

import System.Environment(getArgs)
import Control.Monad (liftM)
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

rearrange :: [a] -> [Int] -> [a]
rearrange xs is = IM.elems $ foldl (\m (x,i) -> IM.insert i x m) IM.empty (xs `zip` is')
        where is' = is ++ [missingElement (length xs) is]
              missingElement n ys = (n*(n+1) `div` 2) - sum ys

processLine :: T.Text -> T.Text
processLine line =
        let [ws,ns] = T.split (==';') line
            w = T.words ws
            n = map (read . T.unpack) $ T.words ns
        in  T.unwords $ rearrange w n

main :: IO ()
main = liftM head getArgs >>= liftM T.lines . TIO.readFile >>= mapM_ (TIO.putStrLn . processLine)

