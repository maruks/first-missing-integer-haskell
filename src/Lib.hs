{-# LANGUAGE OverloadedStrings #-}
module Lib (lowestIntegerNotInList) where

import Data.Int
import Data.Array.Unboxed
import Data.Array.ST (STUArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Control.Monad (when, forM_)
import Data.Array.MArray (newArray)

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R

signed :: Text -> Int32
signed text =
  case R.signed R.decimal text of
    Left e -> error e
    Right (i, _) -> i

decimal :: Text -> Int
decimal text =
  case R.decimal text of
    Left e -> error e
    Right (i, _) -> i

readInput :: IO Text
readInput = TIO.getLine

createNewArray :: Int -> IO (IOUArray Int Int32)
createNewArray size = newArray (1, size) 0

replace :: IOUArray Int Int32 -> Int -> Int -> IO ()
replace stArr idx size =
  when (idx > 0 && idx <= size) $ do
    val <- readArray stArr idx
    when (val /= fromIntegral idx) $ do
      writeArray stArr idx $ fromIntegral idx
      replace stArr (fromIntegral val) size

missingInt :: Int -> Int -> IOUArray Int Int32 -> IO Int32
missingInt i s xs =
  if i <= s then do
    val <- readArray xs i
    if val /= fromIntegral i then return $ fromIntegral i else missingInt (i + 1) s xs
    else return $ fromIntegral $ 1 + s

lowestIntegerNotInList :: IO Int32
lowestIntegerNotInList = do
  size <- decimal <$> readInput
  stArr <- createNewArray size

  forM_ [1 .. size] $ \idx -> do
    i <- signed <$> readInput
    writeArray stArr idx i

  forM_ [1 .. size] $ \idx -> do
    val <- readArray stArr idx
    when (val /= fromIntegral idx) $
      replace stArr (fromIntegral val) size

  missingInt 1 size stArr
