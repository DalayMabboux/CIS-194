{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit i = read . (:[]) . last . show $ i

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit i
        | i < 10 = 0
        | otherwise = read . init . show $ i

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
        | x < 1 = []
        | otherwise = lastDigit x : (toRevDigits $ dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = snd $ foldr (\v (a, xs) -> if a then (False, 2*v:xs)
                                                     else (True, v:xs)) (True, []) l

doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 [] = []
doubleEveryOther2 [x] = [x]
doubleEveryOther2 (x:y:xs) = x:(y*2) : doubleEveryOther2 xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldl (\a d -> a + (sum $ toRevDigits d)) 0


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn d = summe `mod` 10 == 0
         where
           summe = sumDigits . doubleEveryOther2 $ toRevDigits d

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
