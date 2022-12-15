-- cabal install --lib range
import qualified Data.Range as Range
import Data.Range ((+=+))

readInt :: String -> Int
readInt = read

getCoords :: String -> ((Int,Int),(Int,Int))
getCoords s = ((num (w!!2), num(w!!3)),(num(w!!8),num(w!!9)))
  where w = words s
        num = \e -> readInt (takeWhile (\c -> c `elem` "-0123456789") (drop 2 e))

listBeaconNotIn :: ((Int,Int),(Int,Int)) -> Int -> Range.Range Int
listBeaconNotIn ((sx,sy),(bx,by)) y = if rowd > d then ((-1) +=+ (-1))
                                      else ((sx - (d - rowd)) +=+ (sx + (d - rowd)))
  where d = abs (sx - bx) + abs (sy - by)
        rowd = abs (sy - y)

allBeaconIn :: [String] -> Int -> [Range.Range Int]
allBeaconIn l y = foldl (\acc s -> Range.difference acc [listBeaconNotIn (getCoords s) y]) xspace l
  where xspace = [0 +=+ 4000000]

main = do
  d <- readFile "data/15.txt"
  let l = lines d

  print $ map (\(x,y) -> (x!!0)*4000000 + y)
         (filter (\(x,_) -> x /= [])
                 (map (\y -> (Range.fromRanges (allBeaconIn l y), y)) [0..4000000]))
