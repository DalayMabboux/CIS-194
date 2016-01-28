{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P a) == (P b) = r a == r b
       where r z = dropWhile (==0) z
 
-- Exercise 3 -----------------------------------------
rz ::  Poly a -> [(Int, a)]
rz (P a) = reverse $ zip [0..] a

instance (Num a, Eq a, Show a) => Show (Poly a) where
   show (P a) = intercalate " + " . s . reverse $ zip [0..] a
     where s :: (Eq a, Show a, Num a) => [(Int, a)] -> [String]
           s [] = []
           s ((i,z):zs)
             | z == 0 = s zs
             | i == 0 = [(sz z)] ++ s zs
             | i == 1 = [(sz z ++ "x")] ++ s zs
             | otherwise = [(sz z ++ "x^" ++ show i)] ++ s zs
           sz 0 = ""
           sz (-1) = "-"
           sz y = (show y)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

