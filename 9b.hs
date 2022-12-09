import qualified Data.Set as Set

data Point = Point Int Int        -- x, y
  deriving (Show, Eq, Ord)
px :: Point -> Int
px (Point x _) = x
py :: Point -> Int
py (Point _ y) = y

data Snake = Snake [Point] [Point]  -- all knots, tail history
knots :: Snake -> [Point]
knots (Snake knots _) = knots
tailHist :: Snake -> [Point]
tailHist (Snake _ tailHist) = tailHist

move :: Snake -> (Char, Int) -> Snake
move sn (dir,dist) =
  foldl (\sn d -> moveSnake sn d) sn (replicate dist dir)

moveSnake :: Snake -> Char -> Snake
moveSnake sn dir = Snake newKnots (tailHist sn ++ [last newKnots])
  where newKnots = foldl (\kn k -> kn ++ [moveKnot k (last kn)])
                         [(moveHead (head . knots $ sn) dir)]
                         (tail . knots $ sn)

moveHead :: Point -> Char -> Point
moveHead h dir = case dir of
  'U' -> Point (px h) (py h + 1)
  'D' -> Point (px h) (py h - 1)
  'L' -> Point (px h - 1) (py h)
  'R' -> Point (px h + 1) (py h)

moveKnot :: Point -> Point -> Point
moveKnot t h = case (px h - px t, py h - py t) of
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
  d <- readFile "data/9.txt"
  let l = lines d
  let sn = foldl (\sn line -> move sn (parseLine line)) (Snake (replicate 10 (Point 0 0)) [Point 0 0]) l
  print . length . Set.fromList . tailHist $ sn
