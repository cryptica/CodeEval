module Main where

import System.Environment(getArgs)
import System.IO (hFileSize, hClose, openFile, IOMode(ReadMode))

main :: IO ()
main = do
    [inputFile] <- getArgs
    handle <- openFile inputFile ReadMode
    fileSize <- hFileSize handle
    hClose handle
    putStrLn $ show fileSize

