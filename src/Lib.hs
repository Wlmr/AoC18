module Lib
    ( d1a
    , d1b
    ) where

-- DAY 1:

d1a :: String -> String
d1a = show . sum . map readSigned . lines 

d1b :: String -> String
d1b = show . firstDuplicate [0] 0 . cycle . map readSigned . lines 

readSigned :: String -> Int
readSigned s = if '+' == head s then (read . tail) s else read s

firstDuplicate :: [Int] -> Int -> [Int] -> Int
firstDuplicate temp val []     = 0
firstDuplicate temp val (x:xs) = if x' `elem` temp then x' else firstDuplicate (x':temp) x' xs 
        where x' = val+x 

-- DAY 2: