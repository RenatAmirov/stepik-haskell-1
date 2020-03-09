module Demo where

import Data.Char

test = isDigit '7'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y :: Int else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
    where 
        x1 = fst p1
        y1 = snd p1
        x2 = fst p2
        y2 = snd p2