module Main where
import Data.List
import qualified Data.Set as S

part1 :: String -> Bool
part1 pp = length set == length (words pp)
  where set = foldl (flip S.insert) S.empty (words pp)

part2 :: String -> Bool
part2 pp = length set == length (words pp)
  where set = foldl (\acc pw -> S.insert (sort pw) acc) S.empty (words pp)

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ (length . filter part1) input
  print $ (length . filter part2) input
