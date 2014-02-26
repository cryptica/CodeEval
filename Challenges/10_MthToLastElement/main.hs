module Main where

import System.Environment(getArgs)

processLine :: String -> Maybe String
processLine line =
    let (ms:list) = reverse $ words line
        m = read ms - 1
    in  if m < length list then Just (list !! m) else Nothing


maybeIO :: (a -> IO ()) -> Maybe a -> IO ()
maybeIO _ Nothing = return ()
maybeIO f (Just x) = f x

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ (maybeIO putStrLn) $ map processLine $ lines input

