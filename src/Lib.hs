module Lib
    ( d1a
    , d1b
    , d2a
    , d2b
    , d3a
    ) where

import Data.List
import Text.Regex.Posix

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


--day 3
data Square = Square Int Int Int Int deriving Show
type Fabric = [[Int]] 

d3a :: String -> String
d3a = show . twoOrMoreSum . foldl squareAdder t0 . map (square2Fabric size . string2Square) . lines
        where   size = 1000
                t0   = [[0 | x <- [1..size]] | y <- [1..size]]

string2Square :: String -> Square 
string2Square s = Square x y dx dy
    where (x:y:dx:dy:trash) = (tail . map read . concat) (s =~ "[0-9]+" :: [[String]])

square2Fabric :: Int -> Square -> Fabric
square2Fabric size (Square x y dx dy) = pre ++ sqr ++ post
    where pre  = [[0 | col <- [1..size]]    | row <- [1..y]] 
          sqr  = [[0 | col <- [1..x]] ++ [1 | col <- [1..dx]]
               ++ [0 | col <- [x+dx..size]] | row <- [1..dy-1]]
          post = [[0 | col <- [1..size]]    | row <- [y+dy-1..size]] 

squareAdder :: Fabric -> Fabric -> Fabric
squareAdder = zipWith (zipWith (+))

twoOrMoreSum :: Fabric -> Int
twoOrMoreSum = sum . map (length . filter (1<))

d3b :: String -> String 
d3b = show . map (elemIndices 1) . foldl squareAdder t0 . map (square2Fabric size . string2Square) . lines
        where   size = 1000
                t0   = [[0 | x <- [1..size]] | y <- [1..size]]


--day4

--d4a :: String -> String
--d4a = id --lines
--
--d5a :: String -> String
--d5a lst 
--  | [a]      = ""
--  | (a:bc)   = 
--  | (a:b:cd) =   
