module Main where
import Data.Array.IO

between :: Int -> (Int, Int) -> Bool
between i (x, y) = i >= x && i <= y

part1 :: IOArray Int Int -> Int -> Int -> IO Int
part1 mem i steps = do
  j <- readArray mem i
  bounds <- getBounds mem

  if (i + j) `between` bounds then do
    writeArray mem i $ j + 1
    part1 mem (i + j) (steps + 1)
  else
    return steps

part2 :: IOArray Int Int -> Int -> Int -> IO Int
part2 mem i steps = do
  j <- readArray mem i
  bounds <- getBounds mem

  if (i + j) `between` bounds then do
    writeArray mem i $ j + if j > 2 then -1 else 1
    part2 mem (i + j) (steps + 1)
  else
    return steps

main :: IO ()
main = do
  input <- map read . lines <$> getContents

  mem <- newListArray (0, length input - 1) input
  steps <- part1 mem 0 1
  print steps

  mem' <- newListArray (0, length input - 1) input
  steps' <- part2 mem' 0 1
  print steps'
