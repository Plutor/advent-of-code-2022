import Data.List.Split (splitOneOf)

readInt :: String -> Int
readInt = read

parseData :: [String] -> [[Int]]
parseData = map (\e -> map readInt (splitOneOf ",-" e))

main = do
  d <- readFile "data/04.txt"
  let l = parseData . lines $ d
  print l
  let c = length (filter (\v -> (v!!0 <= v!!2 && v!!1 >= v!!3) || (v!!0 >= v!!2 && v!!1 <= v!!3)) l)
  print c