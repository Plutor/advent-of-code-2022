data State = State
  { done :: Bool,
    pos :: Int,
    last3 :: String }


main = do
  d <- readFile "data/6.txt"
  print d
  let r = foldl (\s c -> if done s then s
                         else if elem c (last3 s) || elem ((last3 s)!!0) (drop 1 (last3 s)) || (last3 s)!!1==(last3 s)!!2 then State{done=False, pos=(pos s)+1, last3=(drop 1 (last3 s))++[c]}
                         else State{done=True, pos=(pos s)+2, last3=""}) State{done=False, pos=0, last3=take 3 d} d
  print . pos $ r
