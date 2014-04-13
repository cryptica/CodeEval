module Main where

import System.Environment(getArgs)
import Control.Monad (liftM)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
           deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Date = Date { year :: Int, month :: Month } deriving (Show, Eq, Ord, Bounded)

instance Enum Date where
        fromEnum d = year d * 12 + fromEnum (month d)
        toEnum d = let (y,m) = d `divMod` 12
                   in  Date { year=y, month=toEnum m }

getWorkingExperience :: [(Date,Date)] -> Int
getWorkingExperience xs = months `div` 12
        where
            months = snd $ foldl addInterval (minBound :: Date, 0) $ sort xs
            addInterval (dEnd,time) (d1,d2) =
                  (max dEnd d2, time + timeFromInterval(max (succ dEnd) d1, max dEnd d2))
            timeFromInterval (d1,d2) = (fromEnum d2 - fromEnum d1) + 1

parseDate :: T.Text -> Date
parseDate t =
        let [m,y] = T.split (==' ') t
        in  Date { month=read (T.unpack m), year=read (T.unpack y) }

parseInterval :: T.Text -> (Date,Date)
parseInterval t =
        let [from,to] = T.split (=='-') t
        in  (parseDate from, parseDate to)

processLine :: T.Text -> String
processLine = show . getWorkingExperience . map parseInterval . T.splitOn (T.pack "; ")

main :: IO ()
main = liftM head getArgs >>= liftM T.lines . TIO.readFile >>= mapM_ (putStrLn . processLine)

