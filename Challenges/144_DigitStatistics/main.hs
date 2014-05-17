module Main where

import System.Environment(getArgs)
import Control.Monad
import Data.List
import qualified Data.IntMap as IM

digitRemainders :: IM.IntMap [Int]
digitRemainders = IM.fromList [
            (0,[0]),
            (1,[1]),
            (2,[2,4,8,6]),
            (3,[3,9,7,1]),
            (4,[4,6]),
            (5,[5]),
            (6,[6]),
            (7,[7,9,3,1]),
            (8,[8,4,2,6]),
            (9,[9,1])]

digitStatistics :: Int -> Integer -> [(Int,Integer)]
digitStatistics a n =
        let remainders = digitRemainders IM.! (a `mod` 10)
            (d,m) = n `divMod` genericLength remainders
            countMap = IM.fromList $ zipWith (\x c -> (x,d+c))
                            remainders $ genericReplicate m 1 ++ repeat 0
        in  map (\x -> (x,IM.findWithDefault 0 x countMap)) [0..9]

processLine :: String -> String
processLine line =
        let [as,ns] = words line
            stats = digitStatistics (read as) (read ns)
        in  intercalate ", " $ map (\(d,c) -> show d ++ ": " ++ show c) stats

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

