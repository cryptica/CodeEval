module Main where

import System.Environment(getArgs)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

morseMap :: Map.Map T.Text Char
morseMap =
    let morseList = [
            (".-", 'A'),
            ("-...", 'B'),
            ("-.-.", 'C'),
            ("-..", 'D'),
            (".", 'E'),
            ("..-.", 'F'),
            ("--.", 'G'),
            ("....", 'H'),
            ("..", 'I'),
            (".---", 'J'),
            ("-.-", 'K'),
            (".-..", 'L'),
            ("--", 'M'),
            ("-.", 'N'),
            ("---", 'O'),
            (".--.", 'P'),
            ("--.-", 'Q'),
            (".-.", 'R'),
            ("...", 'S'),
            ("-", 'T'),
            ("..-", 'U'),
            ("...-", 'V'),
            (".--", 'W'),
            ("-..-", 'X'),
            ("-.--", 'Y'),
            ("--..", 'Z'),
            ("-----", '0'),
            (".----", '1'),
            ("..---", '2'),
            ("...--", '3'),
            ("....-", '4'),
            (".....", '5'),
            ("-....", '6'),
            ("--...", '7'),
            ("---..", '8'),
            ("----.", '9')]
    in  Map.fromList (map (\(k,v) -> (T.pack k, v)) morseList)

morseToChar :: T.Text -> Char
morseToChar s = case Map.lookup s morseMap of
    Just c -> c
    Nothing -> error ("Invalid morse symbol: " ++ T.unpack s)

processLine :: T.Text -> String
processLine line =
    let morseWords = T.splitOn (T.pack "  ") $ line
        morseChars = map T.words morseWords
    in  unwords $ map (map morseToChar) morseChars

main :: IO ()
main = do
    [inputFile] <- getArgs
    input <- TIO.readFile inputFile
    mapM_ putStrLn $ map processLine $ T.lines input

