readInt :: String -> Int
readInt = read

main = do
  d <- readFile "data/10.txt"
  let xv = (scanl (+) 1 $ concat (map (\x -> case words x of
                                ["noop"] -> [0]
                                ["addx", x] -> [0, readInt x]) (lines d)))
  print . sum $ map (\x -> x * (xv !! (x-1))) [20, 60, 100, 140, 180, 220]
