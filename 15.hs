import Data.Set (fromList, difference, Set)

readInt :: String -> Int
readInt = read

getCoords :: String -> ((Int,Int),(Int,Int))
getCoords s = ((num (w!!2), num(w!!3)),(num(w!!8),num(w!!9)))
  where w = words s
        num = \e -> readInt (takeWhile (\c -> c `elem` "-0123456789") (drop 2 e))

listBeaconNotIn :: ((Int,Int),(Int,Int)) -> Int -> [Int]
listBeaconNotIn ((sx,sy),(bx,by)) y = if rowd > d then []
                                      else [(sx - (d - rowd)) .. (sx + (d - rowd))]
  where d = abs (sx - bx) + abs (sy - by)
        rowd = abs (sy - y)

allBeaconNotIn :: [String] -> Int -> Set Int
allBeaconNotIn l y = fromList . concat $ map (\s -> listBeaconNotIn (getCoords s) y) l

actualBeaconsIn :: [String] -> Int -> Set Int
actualBeaconsIn l y = fromList (map fst (filter (\(_,by) -> by==y)
                                 (map (\s -> snd (getCoords s)) l)))

main = do
  d <- readFile "data/15.txt"
  let l = lines d

  let b = difference (allBeaconNotIn l 2000000) (actualBeaconsIn l 2000000)
  print . length $ b
