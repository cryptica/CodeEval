module Main where

import System.Environment(getArgs)
import Control.Monad (liftM)
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

monthMap :: M.Map T.Text Int
monthMap = M.fromList $ zip (map T.pack
        ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]) [1..]

type Date = Int

getWorkingExperience :: [(Date,Date)] -> Int
getWorkingExperience = (`div` 12) . snd . foldl addInterval (minBound, 0) . sort
        where addInterval (dEnd,time) (d1,d2) =
                  (max dEnd d2, time + timeFromInterval(max (succ dEnd) d1, max dEnd d2))
              timeFromInterval (d1,d2) = (fromEnum d2 - fromEnum d1) + 1

parseDate :: T.Text -> Date
parseDate t =
        let [m,y] = T.split (==' ') t
        in  fromJust (M.lookup m monthMap) + 12 * read (T.unpack y)

parseInterval :: T.Text -> (Date,Date)
parseInterval t =
        let [from,to] = T.split (=='-') t
        in  (parseDate from, parseDate to)

processLine :: T.Text -> String
processLine = show . getWorkingExperience . map parseInterval . T.splitOn (T.pack "; ")

main :: IO ()
main = liftM head getArgs >>= liftM T.lines . TIO.readFile >>= mapM_ (putStrLn . processLine)

