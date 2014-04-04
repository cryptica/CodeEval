{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import Control.Monad (liftM)
import Control.Applicative ((<|>), (*>))
import Control.Arrow ((&&&))
import Data.Monoid (mappend)
import Data.Word (Word8)
import Data.Char (isDigit)
import Data.List (group,sort,groupBy,sortBy)
import Data.Function (on)
import Data.Bits (Bits,shiftL,shiftR,(.&.),(.|.))

data IP = IP Integer Integer Integer Integer deriving (Eq, Ord)

instance Show IP where
        show (IP d1 d2 d3 d4) = show d1 ++ "." ++ show d2 ++ "." ++ show d3 ++ "." ++ show d4

makeIP :: Integer -> Integer -> Integer -> Integer -> A.Parser IP
makeIP d1 d2 d3 d4 =
        let ip = IP d1 d2 d3 d4
        in  if inRange ip then
                return ip
            else
                fail ("Digits not in range: " ++ show ip)

inRange :: IP -> Bool
inRange ip@(IP d1 d2 d3 d4) =
        all (\d -> 0 <= d && d <= 255) [d1,d2,d3,d4] &&
        ip >= IP 1 0 0 0 && ip <= IP 255 255 255 254

dotted :: A.Parser Integer -> A.Parser IP
dotted p = do
        d1 <- p
        _ <- A.char '.'
        d2 <- p
        _ <- A.char '.'
        d3 <- p
        _ <- A.char '.'
        d4 <- p
        makeIP d1 d2 d3 d4

undotted :: A.Parser Integer -> A.Parser IP
undotted p = do
        i <- p
        let d1 = i `shiftR` 24
            d2 = (i .&. 0x00FF0000) `shiftR` 16
            d3 = (i .&. 0x0000FF00) `shiftR` 8
            d4 = i .&. 0x000000FF
        makeIP d1 d2 d3 d4


takeWith :: Int -> (Word8 -> Bool) -> A.Parser B.ByteString
takeWith n p = do
        s <- A.take n
        if B.all p s
            then return s
            else fail "takeWith"


hexadecimal :: (Integral a, Bits a) => A.Parser a
hexadecimal = B.foldl' step 0 `fmap` AB.takeWhile1 isHexDigit
    where
        isHexDigit w = (w >= 48 && w <= 57) ||
                       (w >= 97 && w <= 102) ||
                       (w >= 65 && w <= 70)
        step a w | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
                 | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
                 | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)

octal :: (Integral a, Bits a) => A.Parser a
octal = B.foldl' step 0 `fmap` AB.takeWhile1 isOctDigit
    where
        isOctDigit w = w >= 48 && w <= 55
        step a w = (a `shiftL` 3) .|. fromIntegral (w - 48)

binary :: (Integral a, Bits a) => A.Parser a
binary = B.foldl' step 0 `fmap` AB.takeWhile1 isBinaryDigit
    where
        isBinaryDigit w = w >= 48 && w <= 49
        step a w = (a `shiftL` 1) .|. fromIntegral (w - 48)



dottedDecimals :: A.Parser IP
dottedDecimals = dotted A.decimal

dottedHexadecimals :: A.Parser IP
dottedHexadecimals = dotted (A.string "0x" *> A.hexadecimal)

dottedOctals :: A.Parser IP
dottedOctals = dotted (A.string "0" *> octal)

dottedBinary :: A.Parser IP
dottedBinary = dotted binary


undottedDecimal :: A.Parser IP
undottedDecimal = undotted A.decimal

undottedHexadecimal :: A.Parser IP
undottedHexadecimal = A.string "0x" *> undotted A.hexadecimal

undottedOctal :: A.Parser IP
undottedOctal = A.string "0" *> undotted octal

undottedBinary :: A.Parser IP
undottedBinary = undotted binary


parseIP :: A.Parser IP
parseIP = dottedBinary <|> dottedOctals <|> dottedHexadecimals <|> dottedDecimals <|>
          undottedBinary <|> undottedOctal <|> undottedHexadecimal <|> undottedDecimal

parseInput :: A.Parser [IP]
parseInput = (do
            ip <- parseIP
            ips <- parseInput
            return (ip:ips)) <|>
        (A.anyChar *> A.skipWhile (not . isDigit) *> parseInput) <|>
        (A.endOfInput *> return [])

processInput :: B.ByteString -> String
processInput input =
        case A.parseOnly parseInput input of
             Right ips ->
                let ipCounts = map (length &&& head) $ group $ sort ips
                    cmpIpC (c1,ip1) (c2,ip2) = (c2 `compare` c1) `mappend` (ip1 `compare` ip2)
                    maxCountIps = map snd $ head $ groupBy ((==) `on` fst) $ sortBy cmpIpC ipCounts
                in  unwords $ map show maxCountIps
             Left e -> e

main :: IO ()
main = liftM processInput (liftM head getArgs >>= B.readFile) >>= putStrLn

