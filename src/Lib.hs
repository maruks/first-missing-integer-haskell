module Lib (lowestIntegerNotInList) where

import Data.Array.Unboxed
import Data.Array.ST (STUArray, newListArray, runSTUArray, readArray, writeArray)
import Data.Array.Base (MArray)
import Control.Monad (when, forM_)

replace :: (MArray a e m, Ix e, Num e) => a e e -> e -> e -> m ()
replace stArr idx size =
  when (idx > 0 && idx <= size) $ do
    val <- readArray stArr idx
    when (val /= idx) $ do
      writeArray stArr idx idx
      replace stArr val size

missingInt' :: Int -> Int -> UArray Int Int -> Int
missingInt' i s xs
  | i <= s = if xs ! i /= i then i else missingInt' (i + 1) s xs
  | otherwise = 1 + s

missingInt :: UArray Int Int -> Int
missingInt xs = let (i,s) = bounds xs in missingInt' i s xs

lowestIntegerNotInList :: [Int] -> Int
lowestIntegerNotInList xs = missingInt $ runSTUArray $ do
  let size = length xs
  stArr <- newListArray (1, size) xs
  forM_ [1 .. size] $ \i -> do
    val <- readArray stArr i
    when (val /= i) $
      replace stArr val size
  return stArr
