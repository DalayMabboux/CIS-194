{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs1 (tail fibs1)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons s l) = s : streamToList l

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons s l) = Cons (f s) (fmap f l)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f i = Cons i (sIterate f (f i))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a s) y = Cons a (sInterleave y s)

sTake :: Int -> Stream a -> [a]
sTake i s = take i $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = rr 0
  where rr i = sInterleave (sRepeat i) (rr (i + 1))

{-
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . .
0     0    0      0     0     0     0     0
   1     2     1     3     1     2     1     4
   1           1           1           1
         2           3           2           4
         2                       2
                     3                       4
-}
-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand s = rr
  where rr = Cons f (rand f)
        f = (1103515245 * s + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 223 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs@(x:_) = Just $ foldl' findMm (x,x) xs
  where findMm a@(mi,mx) i
          | i < mi = (i,mx)
          | i > mx = (mi,i)
          | otherwise = a

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
