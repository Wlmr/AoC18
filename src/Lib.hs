module Lib
    ( d1a
    , d1b
    , d2a
    , d2b
    ) where

import Data.List

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
d2a :: String -> String
d2a = show . uncurry (*) . foldl1 (\(a,b) (x,y) -> (a+x,b+y)) . map (foldl twoAndThreeCounter (0,0) . map length . group . sort) . lines

twoAndThreeCounter :: (Int, Int) -> Int -> (Int, Int)
twoAndThreeCounter (0,  three) 2 = (1,three)
twoAndThreeCounter (two,    0) 3 = (two,1)
twoAndThreeCounter (two,three) _ = (two,three) 

d2b :: String -> String
d2b s = closestMatch (x:xs) xs ++ "\n"
    where (x:xs) = lines s

closestMatch :: [String] -> [String] -> String
closestMatch (x:xs) []     = closestMatch xs xs
closestMatch (x:xs) (y:ys) = if jämförare x y == 1 then commonLetters x y else closestMatch (x:xs) ys

jämförare :: String -> String -> Int
jämförare (x:xs) (y:ys)
    | x == y    = 0 + jämförare xs ys
    | otherwise = 1 + jämförare xs ys
jämförare _ _ = 0

commonLetters :: String -> String -> String
commonLetters (x:xs) (y:ys)
    | x == y    = x : commonLetters xs ys
    | otherwise = "" ++ commonLetters xs ys
commonLetters _ _ = ""