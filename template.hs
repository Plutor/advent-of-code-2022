readInt :: String -> Int
readInt = read

main = do
  d <- readFile "data/1.txt"
  let l = lines d
