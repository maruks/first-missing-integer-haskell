module Lib
  ( lowestIntegerNotInList
  ) where

import Data.Array.Base (STUArray)
import Data.Array.Unboxed (UArray, (!), bounds)
import Data.Array.ST
       (readArray, writeArray, runSTUArray, newListArray)

import Control.Monad.ST (ST)
import Control.Monad (when, forM_)

replace :: STUArray s Int Int -> Int -> Int -> ST s ()
replace stuArray index size =
  when (index > 0 && index <= size) $
  do val <- readArray stuArray index
     when (val /= index) $
       do writeArray stuArray index index
          replace stuArray val size

missingInt' :: Int -> Int -> UArray Int Int -> Int
missingInt' index size uArray
  | index <= size =
    if uArray ! index /= index
      then index
      else missingInt' (index + 1) size uArray
  | otherwise = 1 + size

missingInt :: UArray Int Int -> Int
missingInt uArray =
  let (i, s) = bounds uArray
  in missingInt' i s uArray

lowestIntegerNotInList :: [Int] -> Int
lowestIntegerNotInList xs =
  missingInt $
  runSTUArray $
  do let size = length xs
     stuArray <- newListArray (1, size) xs
     forM_ [1 .. size] $
       \i -> do
         val <- readArray stuArray i
         when (val /= i) $ replace stuArray val size
     return stuArray
