module Main where

day2A :: [Int] -> Int
day2A x = maximum x - minimum x

day2B :: [Int] -> Int
day2B x = head [quot a b | a <- x, b <- x, a > b && a `rem` b == 0]

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  print $ sum $ map day2A input
  print $ sum $ map day2B input
