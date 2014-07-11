module Main where

import System.Environment(getArgs)
import Control.Monad
import qualified Data.Map as M

determineDrops :: M.Map (Int, Int) Int -> Int -> Int -> (M.Map (Int, Int) Int, Int)
determineDrops m eggs floors
        | eggs <= 0 = error "Can not determine drops without eggs"
        | floors <= 2 = (m, floors - 1)
        | eggs == 1 = (m, floors - 1)
        | eggs == 2 = (m, ceiling ((-0.5 :: Float) + sqrt (0.25 + 2 * fromIntegral floors)))
        | otherwise =
            case M.lookup (eggs, floors) m of
                Just drops -> (m, drops)
                Nothing ->
                    let (m', drops) = foldl (\(m0, d0) x ->
                            let (m1,d1) = determineDrops m0 (eggs - 1) x
                                (m2,d2) = determineDrops m1 eggs       (floors - x)
                            in  (m2, min d0 (1 + max d1 d2))
                          )
                          (m, maxBound) [1..(floors `div` 2)]
                    in  (M.insert (eggs, floors) drops m', drops)

processLine :: String -> String
processLine line =
        let [eggs, floors] = map read $ words line
        in  show $ snd $ determineDrops M.empty eggs floors

main :: IO ()
main = liftM head getArgs >>= liftM lines . readFile >>= mapM_ (putStrLn . processLine)

