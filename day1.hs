module Main where

import Data.Char (digitToInt)

day1A :: Char -> String -> Int
day1A first (a:b:t) = day1A first (b:t) + day1A a [b]
day1A first [h]     = if h == first then digitToInt h else 0
day1A _ []          = 0

day1B :: String -> String -> Int
day1B (headOrig:tailOrig) (headShift:tailShift) =
  day1B tailOrig tailShift + if headOrig == headShift then digitToInt headOrig else 0
day1B _ _ = 0

rotate :: [Char] -> [Char]
rotate input = zipWith const (drop halfLength $ cycle input) input
  where halfLength = length input `quot` 2

main :: IO ()
main = do
  input <- getLine
  print $ day1A (head input) input
  print $ day1B input $ rotate input
