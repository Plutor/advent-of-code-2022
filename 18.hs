import Data.List.Split

readInt :: String -> Int
readInt = read

readCoords :: String -> (Int,Int,Int)
readCoords s = (i!!0,i!!1,i!!2)
  where i = map readInt $ splitOn "," s

areAdjacent :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
areAdjacent (x1,y1,z1) (x2,y2,z2) = elem (abs(x1-x2),abs(y1-y2),abs(z1-z2)) [(1,0,0),(0,1,0),(0,0,1)]

main = do
  d <- readFile "data/18.txt"
  let l = map readCoords $ lines d

  print $ (6 * length l) -
    foldl (\cc b -> cc +
      foldl (\c a -> if areAdjacent a b then c+1 else c) 0 l) 0 l