import Data.Char (ord)
import Data.List (elemIndex, elemIndices, nub, sort)

startCoords :: String -> [(Int,Int)]
startCoords topo = map toCoords $ (elemIndices 'S' topo) ++ (elemIndices 'a' topo)

endCoords :: String -> (Int,Int)
endCoords topo = case elemIndex 'E' topo of
    Nothing -> error "no start"
    Just s  -> toCoords s

height :: String -> Int -> Int -> Char
height topo x y
  | x < 0 = '~'
  | y < 0 = '~'
  | x >= 83 = '~'
  | y >= 40 = '~'
  | otherwise = case topo !! ((y*83)+x) of
                     'S' -> 'a'
                     'E' -> 'z'
                     h -> h

visited :: [Bool] -> Int -> Int -> Bool
visited vis x y
  | x < 0 = True
  | y < 0 = True
  | x >= 83 = True
  | y >= 40 = True
  | otherwise = vis !! ((y*83)+x)

toCoords :: Int -> (Int, Int)
toCoords idx = (mod idx 83, div idx 83)

nexts :: String -> ([Bool], [(Int, Int)]) -> ([Bool], [(Int, Int)])
nexts topo (vis, curs) = (nextvis, nextpos)
  where nextpos = nub . concat $ map (\c -> adjToPos topo vis c) curs
        nextvis = map (\(idx,v) -> v || (toCoords idx) `elem` nextpos) (zip [0..] vis)

adjToPos :: String -> [Bool] -> (Int, Int) -> [(Int, Int)]
adjToPos topo vis (x,y) = filter (\(nx,ny) -> (not $ visited vis nx ny) && ord (height topo nx ny) - ord (height topo x y) <= 1)
                             [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

lengthFromStart :: String -> [Bool] -> (Int, Int) -> Int
lengthFromStart topo vis s = if (snd . last $ path) == [] then maxBound else length path
  where path = takeWhile (\(_,c) -> notElem (endCoords topo) c)
                         (scanl (\(v,c) _ -> nexts topo (v,c)) (vis, [s]) [0..408])

main = do
  d <- readFile "data/12.txt"
  let topo = concat . lines $ d
  let vis = replicate (length topo) False

  print . sort $ (map (\s -> lengthFromStart topo vis s) (startCoords topo))

  -- TODO: output includes 363 and 386, which are both impossible.

