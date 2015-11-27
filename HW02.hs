{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = sum . map (\(a,b) -> if a == b then 1 else 0) $ zip xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = [i | c <- colors, let i = length $ elemIndices c xs]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith (\a b -> if a < b then a else b) (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = Move ys (exactMatches xs ys) (matches ys xs - exactMatches xs ys)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs a b) ys = a == a' && b == b'
                                where Move _ a' b' = getMove xs ys

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes x y = filter (isConsistent x) y

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = iter []
allCodes x = concatMap iter (allCodes (x - 1))

iter :: Code -> [Code]
iter c = [ c ++ [n] | n <- colors ]


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = takeWhileInclusive matchingCode . map (getMove x) . allCodes $ length x

matchingCode :: Move -> Bool
matchingCode (Move xs a _) = length xs /= a

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
