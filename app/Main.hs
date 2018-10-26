module Main where

import Lib (lowestIntegerNotInList)

main :: IO ()
main = lowestIntegerNotInList >>= print
