module Main where

import System.Environment(getArgs)
import Control.Monad
import Control.Applicative
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

censorWord :: String -> String
censorWord word = replicate (length word) '_'

changeWord :: String -> String -> Maybe String
changeWord unapproved approved = do
        i <- findIndex (isPrefixOf approved) $ tails unapproved
        return $ censorWord (take i unapproved) ++ approved ++
                 censorWord (drop (i + length approved) unapproved)

changeUtterance :: [String] -> [String] -> Maybe [String]
changeUtterance [] [] = pure []
changeUtterance [] (_:_) = empty
changeUtterance (x:xs) [] = (censorWord x :) <$> changeUtterance xs []
changeUtterance (x:xs) (y:ys) =
        ((:) <$> changeWord x y <*> changeUtterance xs ys) <|>
        ((censorWord x :) <$> changeUtterance xs (y:ys))

processLine :: T.Text -> String
processLine line =
        let [original,altered] = map (words . T.unpack) $ T.split (==';') line
        in  maybe "I cannot fix history" unwords $ changeUtterance original altered

main :: IO ()
main = liftM head getArgs >>= liftM T.lines . TIO.readFile >>= mapM_ (putStrLn . processLine)

