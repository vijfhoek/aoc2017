module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Lazy as M

import Day3 (coords)

part2 :: Int -> (M.Map (Int, Int) Int) -> Int -> Int -> (Int, Int) -> Int
part2 n grid steps (-1) c = part2 n grid (steps +1) (steps `quot` 2) c
part2 n grid steps i (x, y)
  | value < n = part2 n (M.insert (nx, ny) value grid) steps (i - 1) (nx, ny)
  | otherwise = value
  where (nx, ny) = coords steps (x, y)
        value = calcValue grid (nx, ny)

calcValue :: (M.Map (Int, Int) Int) -> (Int, Int) -> Int
calcValue grid (x, y) =
  sum $ mapMaybe (grid M.!?)
    [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
    , (x - 1, y),                 (x + 1, y)
    , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    ]

main :: IO ()
main = do
  n <- read <$> getLine
  let grid = M.singleton (0, 0) 1
  print $ part2 n grid 0 (-1) (0, 0)
