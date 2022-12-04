readInt :: String -> Int
readInt = read

main = do
  d <- readFile "data/DAYHEREDUMMY.txt"
  let l = lines d
