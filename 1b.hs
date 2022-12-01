import Data.List

readInt :: String -> Int
readInt = read

getSums :: [Int] -> String -> [Int]
getSums arr v =
  if v == ""
    then
      arr ++ [0]
    else
      take (length arr - 1) arr ++ [last arr + readInt v]

main = do
  d <- readFile "data/1.txt"
  let l = lines d
  let sums = foldl getSums [0] l
  print . sum . take 3 . reverse . sort $ sums