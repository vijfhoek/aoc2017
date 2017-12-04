module Main where
import Data.List

part1 :: [String] -> Bool
part1 pp = length pp == length (nub pp)

part2 :: [String] -> Bool
part2 pp = length pp == length (nub $ map sort pp)

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ (length . filter part1) $ map words input
  print $ (length . filter part2) $ map words input
