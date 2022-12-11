readInt :: String -> Int
readInt = read

main = do
  d <- readFile "data/10.txt"
  let x = scanl (+) 1 $ concat (map (\x -> case words x of
                                ["noop"] -> [0]
                                ["addx", x] -> [0, readInt x]) (lines d))
  putStr . concat $ map (\(x,i) -> (if abs ((mod i 40)-x) <= 1 then "OK" else "  ") ++
                                   (if (mod (i+1) 40) == 0 then "\n" else "")) (zip x [0..])

