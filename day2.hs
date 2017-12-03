module Main where

part1 :: [Int] -> Int
part1 x = maximum x - minimum x

part2 :: [Int] -> Int
part2 x = head [quot a b | a <- x, b <- x, a > b && a `rem` b == 0]

day2 :: ([Int] -> Int) -> [[Int]] -> IO ()
day2 f input = print $ (sum . map f) input

main :: IO ()
main = do
  input <- map (map read . words) . lines <$> getContents
  day2 part1 input
  day2 part2 input
