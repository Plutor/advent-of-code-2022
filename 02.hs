import Data.Char (ord)
import Data.Set (member, fromList)

shapeScore :: [Char] -> Int
shapeScore m = ord (last m) - ord 'W'

outcomeScore :: [Char] -> Int
outcomeScore m = if member m (fromList ["A X", "B Y", "C Z"]) then 3 -- tie
  else if member m (fromList ["A Y", "B Z", "C X"]) then 6 -- win
  else 0

main = do
  d <- readFile "data/02.txt"
  let l = lines d
  let sc = foldl (\acc m -> acc + shapeScore m + outcomeScore m) 0 l
  print sc
