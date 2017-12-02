module Main where

import Data.Char (digitToInt)

day1A :: [Int] -> Int
day1A (a:b:t) = day1A (b:t) + if a == b then a else 0
day1A _       = 0

day1B :: [Int] -> [Int] -> Int
day1B (a:at) (b:bt) = day1B at bt + if a == b then a else 0
day1B _ _           = 0

rotate :: [Int] -> [Int]
rotate x = drop (length x `quot` 2) $ cycle x

main :: IO ()
main = do
  input <- map digitToInt <$> getLine
  print $ day1A (head input:input)
  print $ day1B input $ rotate input
