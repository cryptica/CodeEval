{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment(getArgs)
import Control.Monad
import Control.Applicative
import Data.Word
import Data.Bits
import Data.Fixed

import Data.Attoparsec.Char8
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- | Consume @n@ bytes of input, but succeed only if the predicate
-- returns 'True'.
takeWith :: Int -> (Word8 -> Bool) -> Parser B.ByteString
takeWith n p = do
        s <- AB.take n
        if B.all p s
            then return s
            else fail "takeWith"

-- | Parse and decode an unsigned hexadecimal number of length @n@.
hexadecimalN :: (Integral a, Bits a) => Int -> Parser a
hexadecimalN n = B.foldl' step 0 `fmap` takeWith n isHexDigit
    where
        isHexDigit w = (w >= 48 && w <= 57) ||
                       (w >= 97 && w <= 102) ||
                       (w >= 65 && w <= 70)
        step a w | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
                 | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
                 | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)

data Color = HSL Float Float Float
           | HSV Float Float Float
           | CMYK Float Float Float Float
           | RGB Word8 Word8 Word8
        deriving Show

toRGB :: Color -> (Word8,Word8,Word8)
toRGB (HSL h s l) =
        let c = (1 - abs (2*l - 1)) * s
            x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
            m = l - c/2
            (r',g',b') | 0 <= h && h <  60 = (c,x,0)
                       | 60 <= h && h < 120 = (x,c,0)
                       | 120 <= h && h < 180 = (0,c,x)
                       | 180 <= h && h < 240 = (0,x,c)
                       | 240 <= h && h < 300 = (x,0,c)
                       | 300 <= h && h < 360 = (c,0,x)
                       | otherwise          = (0,0,0)
        in  (round (255*(r' + m)),
             round (255*(g' + m)),
             round (255*(b' + m)))
toRGB (HSV h s v) =
        let c = v * s
            x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
            m = v - c
            (r',g',b') | 0 <= h && h <  60 = (c,x,0)
                       | 60 <= h && h < 120 = (x,c,0)
                       | 120 <= h && h < 180 = (0,c,x)
                       | 180 <= h && h < 240 = (0,x,c)
                       | 240 <= h && h < 300 = (x,0,c)
                       | 300 <= h && h < 360 = (c,0,x)
                       | otherwise          = (0,0,0)
        in  (round (255*(r' + m)),
             round (255*(g' + m)),
             round (255*(b' + m)))
toRGB (CMYK c m y k) = (round (255*(1-c)*(1-k)),
                        round (255*(1-m)*(1-k)),
                        round (255*(1-y)*(1-k)))
toRGB (RGB r g b) = (r,g,b)

parseColor :: Parser Color
parseColor = parseHSL <|> parseHSV <|> parseCMYK <|> parseRGB <|> parseHex

parseRationals :: RealFloat a => Parser [a]
parseRationals = do
        _ <- char '('
        xs <- sepBy1 rational (char ',')
        _ <- char ')'
        return xs

parseHSL :: Parser Color
parseHSL = do
        _ <- string "HSL"
        [h,s,l] <- parseRationals
        return $ HSL h (s/100) (l/100)

parseHSV :: Parser Color
parseHSV = do
        _ <- string "HSV"
        [h,s,v] <- parseRationals
        return $ HSV h (s/100) (v/100)

parseCMYK :: Parser Color
parseCMYK = do
        [c,m,y,k] <- parseRationals
        return $ CMYK c m y k

parseHex :: Parser Color
parseHex = do
        _ <- string "#"
        r <- hexadecimalN 2
        g <- hexadecimalN 2
        b <- hexadecimalN 2
        return $ RGB r g b

parseRGB :: Parser Color
parseRGB = do
        _ <- string "RGB("
        r <- decimal
        _ <- char ','
        g <- decimal
        _ <- char ','
        b <- decimal
        _ <- string ")"
        return $ RGB r g b

processLine :: B.ByteString -> String
processLine line = case parseOnly parseColor line of
             Right color -> "RGB" ++ show (toRGB color)
             Left e -> e

main :: IO ()
main = liftM head getArgs >>= liftM BC.lines . BC.readFile >>= mapM_ (putStrLn . processLine)

