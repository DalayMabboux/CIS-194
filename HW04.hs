{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

-- instance (Num a, Eq a) => Eq (Poly a) where
--     (P a) == (P b) = r a == r b
--        where r z = dropWhile (==0) z

-- Exercise 3 -----------------------------------------
-- instance (Num a, Eq a, Show a) => Show (Poly a) where
--     show (P a) = show a
instance (Num a, Eq a, Show a) => Show (Poly a) where
   show (P a) = intercalate " + " . s . reverse $ zip [0..] a
     where s :: (Eq a, Show a, Num a) => [(Int, a)] -> [String]
           s [] = []
           s ((i,z):zs)
             | z == 0 = s zs
             | i == 0 = sz z : s zs
             | i == 1 = (sz z ++ "x") : s zs
             | otherwise = (sz z ++ "x^" ++ show i) : s zs
           sz 0 = ""
           sz y = show y

-- Exercise 4 -----------------------------------------
-- postfixZero :: Num a => Int -> [a] -> [a]
-- postfixZero c l = l ++ replicate c 0

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ zipWith (+) (f a b) (f b a)
  where f m n
          | length m < length n = postfixZero (length n - length m) m
          | otherwise = m
        postfixZero c l = l ++ replicate c 0

-- Exercise 5 -----------------------------------------
mulShift :: Num a => Int -> [a] -> [a] -> [[a]]
mulShift _ [] _ = []
mulShift s (a:as) p2s = (replicate s 0 ++ map (*a) p2s) : mulShift (s+1) as p2s

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sumPolyList polyList
          where polyList = mulShift 0 a b
                sumPolyList (c:ps) = foldr (\n accu -> accu `plus` P n) (P c) ps

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map negate a
    fromInteger b = P [fromInteger b]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined


-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P b) z = sum $ map (\(i,v) -> v * (z^i)) $ zip [0..] b

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n b = iterate deriv b !! n

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P z) = P . tail $ map (\(i,v) -> i * v) $ zip [0..] z
