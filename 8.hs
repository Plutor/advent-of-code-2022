rotate :: [a] -> [a]
rotate f = foldl (\out i -> out ++ [f !! ((mod i 99)*99 + (div i 99))]) [] [0..9801]

markVisiblesFromLeft :: String -> [Bool] -> [Bool]
markVisiblesFromLeft f v = fst (foldl
  (\(vl,mx) (i,h) -> if (mod i 99) == 0 || h > mx then (vl ++ [True], h)
                     else (vl ++ [v !! i], mx))
  ([], '/')
  (zip [0..] f))

main = do
  d <- readFile "data/8.txt"
  let f = concat . lines $ d
  let v = snd (foldl (\(fo,v) _ -> (rotate fo, rotate ( markVisiblesFromLeft fo v )))
                     (f, replicate (length f) False)
                     [0..3])
  print . length $ (filter (==True) v)

-- 1059 is too low