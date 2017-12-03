module Day3 where

coords :: Int -> (Int, Int) -> (Int, Int)
coords steps (x, y) = case (steps - 1) `rem` 4 of
  0 -> (x + 1, y)
  1 -> (x, y + 1)
  2 -> (x - 1, y)
  _ -> (x, y - 1)
