module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R

import Control.Monad (forM_)
import Data.Array.MArray (newArray, writeArray, getElems)
import Data.Array.IO (IOUArray)

import Lib (lowestIntegerNotInList)

signed :: Text -> Int
signed text =
  case R.signed R.decimal text of
    Left e -> error e
    Right (i, _) -> i

decimal :: Text -> Int
decimal text =
  case R.decimal text of
    Left e -> error e
    Right (i, _) -> i

readInput :: IO [Int]
readInput = do
  size <- decimal <$> TIO.getLine
  ioArray <- newArray (1, size) 0 :: IO (IOUArray Int Int)
  forM_ [1 .. size] $
    \idx -> do
      i <- signed <$> TIO.getLine
      writeArray ioArray idx i
  getElems ioArray

main :: IO ()
main = lowestIntegerNotInList <$> readInput >>= print
