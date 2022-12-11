import Data.List (sort)

data Monkey = Monkey {
  items :: [Int],
  op :: Int -> Int,
  test :: Int -> Int,
  count :: Int
}
instance Show Monkey where
  show (Monkey i _ _ _) = show i

tossItems :: Monkey -> [(Int, Int)]  -- (new worry, new monkey)
-- 9699690 = 11*3*2*17*7*5*13*19
tossItems m = map (\o -> (mod (op m o) 9699690, test m (op m o))) (items m)

doMonkeyRound :: [Monkey] -> [Monkey]
doMonkeyRound monkeys = last (scanl (doMonkeyTurn) monkeys [0..(length monkeys)-1])

doMonkeyTurn :: [Monkey] -> Int -> [Monkey]
doMonkeyTurn monkeys n = map (\(i,m) -> Monkey{items=(items m) ++ (map fst (filter (\p -> (snd p)==i) toss)),
                                               op=op m, test = test m, count=count m})
                             (zip [0..] (take (n) monkeys))
                         ++ [Monkey{items=[], op=op (monkeys !! n), test = test (monkeys !! n), count=count (monkeys !! n) + (length . items $ (monkeys !! n))}]
                         ++ map (\(i,m) -> Monkey{items=(items m) ++ (map fst (filter (\p -> (snd p)==i) toss)),
                                               op=op m, test = test m, count=count m})
                             (zip [n+1..] (drop (n+1) monkeys))
  where toss = tossItems (monkeys !! n)

main = do
  let monkeys = [ Monkey{
                    items = [93, 98],
                    op = \v -> v * 17,
                    test = \v -> if mod v 19 == 0 then 5 else 3,
                    count = 0
                  },
                  Monkey{
                    items = [95, 72, 98, 82, 86],
                    op = \v ->v + 5,
                    test = \v -> if mod v 13 == 0 then 7 else 6,
                    count = 0
                  },
                  Monkey{
                    items = [85, 62, 82, 86, 70, 65, 83, 76],
                    op = \v ->v + 8,
                    test = \v -> if mod v 5 == 0 then 3 else 0,
                    count = 0
                  },
                  Monkey{
                    items = [86, 70, 71, 56],
                    op = \v ->v + 1,
                    test = \v -> if mod v 7 == 0 then 4 else 5,
                    count = 0
                  },
                  Monkey{
                    items = [77, 71, 86, 52, 81, 67],
                    op = \v ->v + 4,
                    test = \v -> if mod v 17 == 0 then 1 else 6,
                    count = 0
                  },
                  Monkey{
                    items = [89, 87, 60, 78, 54, 77, 98],
                    op = \v ->v * 7,
                    test = \v -> if mod v 2 == 0 then 1 else 4,
                    count = 0
                  },
                  Monkey{
                    items = [69, 65, 63],
                    op = \v ->v + 6,
                    test = \v -> if mod v 3 == 0 then 7 else 2,
                    count = 0
                  },
                  Monkey{
                    items = [89],
                    op = \v ->v * v,
                    test = \v -> if mod v 11 == 0 then 0 else 2,
                    count = 0
                  }]

  let lastturn = (iterate doMonkeyRound monkeys) !! 10000
  print . product . take 2 . reverse . sort $ map count lastturn
