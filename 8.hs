readInt :: String -> Int
readInt = read

-- TODO implement
rotate :: String -> String
rotate f = f

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
  let l = concat . lines $ d
  print (visibleInRow l 23 1)
  -- TODO: for each (x, y), count (visibleInRow x y || visibleInRow (rotate f) y x)
