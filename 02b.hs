import Data.Char (ord)
import Data.Set (member, fromList)

shapeScore :: [Char] -> Int
shapeScore m = if member m (fromList ["A Y", "B X", "C Z"]) then 1 -- rock
  else if member m (fromList ["A Z", "B Y", "C X"]) then 2 -- paper
  else 3  -- scissors

outcomeScore :: [Char] -> Int
outcomeScore m = (ord (last m) - ord 'X') * 3

main = do
  d <- readFile "data/02.txt"
  let l = lines d
  let sc = foldl (\acc m -> acc + shapeScore m + outcomeScore m) 0 l
  print sc
