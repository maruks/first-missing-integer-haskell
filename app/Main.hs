module Main where

import Data.Text (Text, split)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R

import Lib (lowestIntegerNotInList)

parseText :: Text -> Int
parseText text =
  case R.signed R.decimal text of
    Left e -> error e
    Right (i, _) -> i

readInput :: IO [Int]
readInput = do
  line <- TIO.getLine
  return $ map parseText $ split (== ',') line

main :: IO ()
main = do
  st <- readInput
  print $ lowestIntegerNotInList st
