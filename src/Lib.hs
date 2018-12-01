module Lib
    ( d1
    , d1b
    ) where

d1 :: String -> String
d1 = show . sum . map readSigned . lines 
        where readSigned sInt = if '+' == head sInt then (read . tail) sInt else read sInt

d1b :: String -> String
d1b = show . firstToTwo [0] 0 . cycle . map readSigned . lines 
        where readSigned signInt = if '+' == head signInt then (read . tail) signInt else read signInt

--helper for d1b
firstToTwo :: [Int] -> Int -> [Int] -> Int
firstToTwo temp val []     = 0
firstToTwo temp val (x:xs) = if x' `elem` temp then x' else firstToTwo (x':temp) x' xs 
        where x' = val+x 
