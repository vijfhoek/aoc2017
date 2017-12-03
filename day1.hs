module Main where

import Data.Char (digitToInt)

part1 :: [Int] -> Int
part1 (a:b:t) = part1 (b:t) + if a == b then a else 0
part1 _       = 0

part2 :: [Int] -> [Int] -> Int
part2 (a:at) (b:bt) = part2 at bt + if a == b then a else 0
part2 _ _           = 0

rotate :: [Int] -> [Int]
rotate x = drop (length x `quot` 2) $ cycle x

main :: IO ()
main = do
  input <- map digitToInt <$> getLine
  print $ part1 (head input:input)
  print $ part2 input $ rotate input
