readInt :: String -> Int
readInt = read

rotate :: String -> String
rotate f = foldl (\out i -> out ++ [f !! ((mod i 4)*4 + (div i 4))]) "" [0..]

visibleInRow :: String -> Int -> Int -> Bool
visibleInRow f _ 0 = True
visibleInRow f _ 98 = True
visibleInRow f 0 _ = True
visibleInRow f 98 _ = True
visibleInRow f x y = fst (foldl
  (\(v,mx) (tx,h) -> if v                    then (v,mx)
                     else if tx==x && h > mx then (True,mx)
                     else if tx<x  && h > mx then (v,h)
                     else if tx>x  && h > mx then (False,mx)
                     else (v,mx))
  (False, '/')
  (zip [0..] (take 99 (drop (99*y) f))))

main = do
  d <- readFile "data/8.txt"
  let f = concat . lines $ d
  print . sum $ (map (\i -> if visibleInRow f (mod i 99) (div i 99) || visibleInRow (rotate f) (mod i 99) (div i 99) then 1 else 0) [0..9801])
