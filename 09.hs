import qualified Data.Set as Set

data Point = Point Int Int        -- x, y
  deriving (Show, Eq, Ord)
px :: Point -> Int
px (Point x _) = x
py :: Point -> Int
py (Point _ y) = y

data Snake = Snake Point [Point]  -- head, tail history
hd :: Snake -> Point
hd (Snake h _) = h
tailHist :: Snake -> [Point]
tailHist (Snake _ tailHist) = tailHist

move :: Snake -> (Char, Int) -> Snake
move sn (dir,dist) =
  foldl (\sn d -> Snake (moveHead (hd sn) d) ((tailHist sn) ++ [moveTail (last . tailHist $ sn) (moveHead (hd sn) d)])) sn (replicate dist dir)

moveHead :: Point -> Char -> Point
moveHead h dir = case dir of
  'U' -> Point (px h) (py h + 1)
  'D' -> Point (px h) (py h - 1)
  'L' -> Point (px h - 1) (py h)
  'R' -> Point (px h + 1) (py h)

moveTail :: Point -> Point -> Point
moveTail t h = case (px h - px t, py h - py t) of
  (2,2) -> Point (px t + 1) (py t + 1)
  (2,1) -> Point (px t + 1) (py t + 1)
  (1,2) -> Point (px t + 1) (py t + 1)
  (-2,2) -> Point (px t - 1) (py t + 1)
  (-2,1) -> Point (px t - 1) (py t + 1)
  (-1,2) -> Point (px t - 1) (py t + 1)
  (2,-2) -> Point (px t + 1) (py t - 1)
  (2,-1) -> Point (px t + 1) (py t - 1)
  (1,-2) -> Point (px t + 1) (py t - 1)
  (-2,-2) -> Point (px t - 1) (py t - 1)
  (-2,-1) -> Point (px t - 1) (py t - 1)
  (-1,-2) -> Point (px t - 1) (py t - 1)
  (2,0) -> Point (px t + 1) (py t)
  (-2,0) -> Point (px t - 1) (py t)
  (0,2) -> Point (px t) (py t + 1)
  (0,-2) -> Point (px t) (py t - 1)
  (_,_) -> t

parseLine :: String -> (Char, Int)
parseLine l = ((words l)!!0!!0, read ((words l)!!1))

main = do
  d <- readFile "data/09.txt"
  let l = lines d
  let sn = foldl (\sn line -> move sn (parseLine line)) (Snake (Point 0 0) [Point 0 0]) l
  print . length . Set.fromList . tailHist $ sn
