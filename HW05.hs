{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Data.Maybe (fromJust)
import Data.List

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fp1 fp2 = do
              f1 <- BS.readFile fp1
              f2 <- BS.readFile fp2
              return $ filterSecret f1 f2

-- filteredWord8
filterSecret :: ByteString -> ByteString -> ByteString
filterSecret f1 f2 = BS.concat $ map BS.singleton filteredWord8
                        where filteredWord8 = filter (/= 0) $ BS.zipWith xor f1 f2

-- Exercise 2 -----------------------------------------
-- Get the secret as ByteString and the encoded file as ByteString and return the secret replicated as many times as it matches the size
-- of the given encoded ByteString length
getSecretByteString :: ByteString -> ByteString -> ByteString
getSecretByteString secret encoded = BS.concat $ replicate c secret
                                        where c = 1 + fromIntegral (BS.length encoded `div` BS.length secret)

-- Get the secret as ByteString and the encoded file as ByteString, XOR them and return the result as ByteString
xorSecret :: ByteString -> ByteString -> ByteString
xorSecret s ebs = BS.pack $ BS.zipWith xor ebs (getSecretByteString s ebs)

-- Provide the secret as ByteString and the path to the encryped file. When the action runs, it creates a file (fp) with the suffix "enc"
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey s fp = do
                        esb <- BS.readFile $ fp ++ ".enc"
                        BS.writeFile fp $ xorSecret s esb

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
                  bs <- BS.readFile fp
                  return (decode bs)

-- Exercise 4 -----------------------------------------
filterBadTransaction :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterBadTransaction (Just tids) (Just ts) = Just (filter (\t -> tid t `elem` tids) ts)
filterBadTransaction _ _ = Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vfp tvp = do
  vj <- parseFile vfp
  tj <- parseFile tvp
  return $ filterBadTransaction vj tj

-- Exercise 5 -----------------------------------------
-- Update the map for the given Transaction: add the amount to the 'to' person and remove it from the 'from' person
updateMap :: Transaction -> Map String Integer -> Map String Integer
updateMap t m = Map.insertWith (+) (to t) (amount t) $ Map.insertWith (+) (from t) (- amount t) m

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr updateMap Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = snd $ Map.foldlWithKey (\(a,o) k n -> if n > a then (n,k) else (a,o)) (0,"") m

-- Exercise 7 -----------------------------------------
prc :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
prc [] _ _ ts = ts
prc _ [] _ ts = ts
prc w@(w1:ws) l@(l1:ls) tids ts
        | snd w1 == 0 = prc ws l tids ts -- if the payer has 0 left, remove it and continue
        | snd l1 == 0 = prc w ls tids ts -- if the payee has 0 left, remove it and continue
        | snd w1 + snd l1 > 0 = prc ((fst w1, snd w1 + snd l1) : ws) ls tids (t (- snd l1) : ts) -- if the amount from the payer is higher than the amount of the payee, substract the amount from payer and remove the payee from the list
        | snd w1 + snd l1 == 0 = prc ws ls tids (t (- snd l1) : ts) -- if the amount of payer and payee are equal, remove both from the list
        | otherwise = prc ws ((fst l1, snd l1 + snd w1) : ls) tids (t (snd w1) : ts) -- if the amount of the payee is higher then add the amount to the payee and remove the payer from the list
        where t a = Transaction {from = fst w1, to = fst l1, tid = head tids, amount = a} -- create a Transaction with the given amount

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = prc ws ls tids []
        where ws = reverse . sortOn snd $ Map.toAscList payers
              ls = sortOn snd $ Map.toAscList payees
              payers = fst $ Map.partition (>=0) m
              payees = snd $ Map.partition (>=0) m

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp a = writeFile fp (show (encode a))

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
