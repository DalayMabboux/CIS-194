{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

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
                                        where c = 1 + (fromIntegral $ BS.length encoded `div` BS.length secret)

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
parseFile = undefined

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs = undefined

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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
