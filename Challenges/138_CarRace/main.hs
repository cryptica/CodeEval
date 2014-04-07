{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Printf (printf)

data Section = Section { distance :: Float, vEndCoeff :: Float } deriving Show
type Track = [Section]
data Car = Car { num :: Int, vMax :: Float, aPos :: Float, aNeg :: Float } deriving Show

--raceThroughSection :: Car -> (Float, Float) -> Section -> (Float, Float)
--raceThroughSection car (tStart, vStart) section =
--        let vEnd = vEndCoeff section * vMax car
--            dt1 = (vMax car - vStart) / aPos car
--            dt3 = (vMax car - vEnd) / aNeg car
--            dt2 = (distance section - 0.5 * (dt1 * (vStart + vMax car) + dt3 * (vEnd + vMax car)))
--                    / vMax car
--        in (tStart + dt1 + dt2 + dt3, vEnd)

raceThroughSection :: Car -> (Float, Float) -> Section -> (Float, Float)
raceThroughSection car (tStart, vStart) section =
        let vEnd = vEndCoeff section
            tEnd = tStart + distance section / vMax car +
                     0.5 * (aPos car * (1 - vStart)**2 + aNeg car * (1 - vEnd)**2)
        in (tEnd, vEnd)

raceThroughTrack :: Track -> Car -> Float
raceThroughTrack track car = fst $ foldl (raceThroughSection car) (0,0) track

parseTrack :: [String] -> Track
parseTrack [] = []
parseTrack (l:d:xs) = Section { distance=read l, vEndCoeff=(180 - read d) / 180 }:parseTrack xs

--parseCar :: [String] -> Car
--parseCar [n,ms,ap,an] =
--        let vm = read ms / 3600
--        in  Car { num=read n, vMax=vm, aPos=vm / read ap, aNeg=vm / read an }

parseCar :: [String] -> Car
parseCar [n,ms,ap,an] = Car { num=read n, vMax=read ms / 3600, aPos=read ap, aNeg=read an }

processInput :: String -> String
processInput input =
        let (trackS:carsS) = lines input
            track = parseTrack $ words trackS
            cars = map (parseCar . words) carsS
            times = map (raceThroughTrack track) cars
            carTimes = sortBy (comparing snd) (zip cars times)
            carTimesS = map (\(car,time) -> printf "%d %.2f" (num car) time) carTimes
        in  unlines carTimesS

main :: IO ()
main = liftM processInput (liftM head getArgs >>= readFile) >>= putStrLn

