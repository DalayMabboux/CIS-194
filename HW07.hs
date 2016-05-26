{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, snoc, (!), (!?), (//), head, tail)
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV idxA idxB v = do
  let mvA = v !? idxA
  let mvB = v !? idxB
  vA <- mvA
  vB <- mvB
  return $ v // [(idxA, vB),(idxB, vA)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = foldr m (return [])
  where m n o = do
          r <- f n -- apply the function to the current entry in the list
          om <- o  -- get the list out of the monad
          return (r:om) -- add the value to the front of the list

getElts :: [Int] -> Vector a -> Maybe [a]
getElts as v = mapM (v !?) as

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  i <- getRandomR (0, length v - 1)
  return $ v !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec i = do
  rs <- getRandoms
  return $ V.fromList $ take i rs

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR i (l,u) = do
  rs <- getRandomRs (l,u)
  return $ V.fromList $ take i rs

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle as = do
  let l = V.length as
  let i = [0 .. l - 2] -- all elements with this indexes will be shuffled; there is no more element beyond the last element, so don't shuffle the last element
  let z = mapM (\a -> getRandomR (a + 1, l - 1)) i -- genereate a random Int; bounds = current index + 1 to length - 1
  y <- z -- get the list of random Int out of the RandT
  let x = zip i y -- [(0,x),(1,x),(2,x),(3,x), ... (length - 2,x)]
  let v = foldl sw as x
  return v
    where
      sw xs (a,b) = xs // [(a, xs ! b),(b, xs ! a)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt as p = (bs, pivot, us)
  where
    (bs,us, _) = V.foldl move (V.fromList [], V.fromList [], False) as
    move (bs',us',added) a
      | a < pivot = (V.snoc bs' a, us', added)
      | otherwise = (bs', fst (addToUpper us' a added), snd (addToUpper us' a added))
    pivot = as ! p
    addToUpper us' a added
      | added && a == pivot = (V.snoc us' a, added)
      | not added && a == pivot = (us', True)
      | otherwise = (V.snoc us' a, added)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vs
  | V.null vs = V.empty
  | otherwise = qsort [v | v <- rest, v < first] <> V.cons first (qsort [v | v <- rest, v >= first])
  where first = V.head vs
        rest = V.tail vs

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vs
  | V.null vs = return V.empty
  | otherwise = do
      let lgt = V.length vs
      pivot <- getRandomR (0, lgt - 1)
      let (l,p,u) = partitionAt vs pivot
      ql <- qsortR l
      qu <- qsortR u
      return $ ql <> V.cons p qu

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vs
  | len == 0 || i >= len = return Nothing
  | otherwise = do
    g <- getRandomR (0, V.length vs - 1)
    let (l,p,u) = partitionAt vs g
    let ll = V.length l
    if i == ll then
      return (Just p)
    else
      if i < ll then
        select i l
      else
        select (i - ll - 1) u
    where
      len = V.length vs

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d
  | n > V.length d = Nothing
  | n == 0 || V.null d = Just ([], d)
  | otherwise = do
    t <- nextCard d
    r <- getCards (n - 1) (snd t)
    return (fst t : fst r, snd r)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
