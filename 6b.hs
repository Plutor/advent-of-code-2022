import Data.Set (fromList)

data State = State
  { done :: Bool,
    pos :: Int,
    last13 :: String }


main = do
  d <- readFile "data/6.txt"
  print d
  let r = foldl (\s c -> if done s then s
                         else if length (fromList (last13 s++[c])) /= 14 then State{done=False, pos=(pos s)+1, last13=(drop 1 (last13 s))++[c]}
                         else State{done=True, pos=(pos s)+1, last13=""}) State{done=False, pos=0, last13=take 13 d} d
  print . pos $ r
