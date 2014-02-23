import System.Environment

type Outline = [(Int, Int)]

splitBy :: Char -> String -> [String]
splitBy c s =
    case dropWhile (== c) s of
        "" -> []
        s' -> w : splitBy c s''
            where (w, s'') = break (== c) s'

mergeOutlines' :: Int -> Int -> Outline -> Outline -> Outline
mergeOutlines' _ _ [] o2 = o2
mergeOutlines' _ _ o1 [] = o1
mergeOutlines' ch1 ch2 o1@((x1,h1):os1) o2@((x2,h2):os2) =
    let o1'  = if x1 <= x2 then os1 else o1
        o2'  = if x2 <= x1 then os2 else o2
        ch1' = if x1 <= x2 then h1  else ch1
        ch2' = if x2 <= x1 then h2  else ch2
        os   = mergeOutlines' ch1' ch2' o1' o2'
    in  if max ch1' ch2' == max ch1 ch2 then os
        else (min x1 x2, max ch1' ch2') : os

mergeOutlines :: Outline -> Outline -> Outline
mergeOutlines = mergeOutlines' 0 0

mergeAll :: [Outline] -> Outline
mergeAll = foldl mergeOutlines []

buildingToOutline :: (Int, Int, Int) -> Outline
buildingToOutline (x1, h1, x2) = [(x1, h1), (x2, 0)]

stringToBuildings :: String -> [(Int, Int, Int)]
stringToBuildings s = map read $ splitBy ';' s

outlineToList :: Outline -> [Int]
outlineToList [] = []
outlineToList ((x1,h1):os) = x1 : h1 : outlineToList os

outlineToString :: Outline -> String
outlineToString = unwords . map show . outlineToList

processLine :: String -> String
processLine = outlineToString . mergeAll . map buildingToOutline . stringToBuildings

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- readFile inputFile
    mapM_ putStrLn $ map processLine $ lines input

