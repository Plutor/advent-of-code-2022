import Data.List.Split (splitOneOf)

readInt :: String -> Int
readInt = read

parseData :: [String] -> [[Int]]
parseData = map (\e -> map readInt (splitOneOf ",-" e))

main = do
  d <- readFile "data/04.txt"
  let l = parseData . lines $ d
  let c = length (filter (\v -> not (v!!0 < v!!2 && v!!0 < v!!3 && v!!1 < v!!2 && v!!1 < v!!3) && not (v!!0 > v!!2 && v!!0 > v!!3 && v!!1 > v!!2 && v!!1 > v!!3)) l)
  print c
