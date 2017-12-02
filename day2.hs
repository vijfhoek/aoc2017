module Main where

day2A :: [[Int]] -> Int
day2A input = sum $ map (\x -> maximum x - minimum x) input

day2B :: [[Int]] -> Int
day2B input = sum $ map (\x -> head [quot a b | a <- x, b <- x, a > b && a `rem` b == 0]) input

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  print $ day2A input
  print $ day2B input
