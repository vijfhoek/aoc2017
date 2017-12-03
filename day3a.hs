module Main where
import Day3 (coords)

part1 :: Int -> Int -> Int -> (Int, Int) -> Int
part1 n steps (-1) c = part1 n (steps + 1) (steps `quot` 2) c
part1 n steps i (x, y)
  | n > 0     = part1 (n - 1) steps (i - 1) (coords steps (x, y))
  | otherwise = abs x + abs y

main :: IO ()
main = do
  n <- read <$> getLine
  print $ part1 (n - 1) 0 (-1) (0, 0)
