import Data.Char (ord)
import Data.List.Split  -- cabal install --lib split
import Data.Set (fromList, intersection, elemAt)

commonItem :: [[Char]] -> Char
commonItem abc = elemAt 0 (intersection (intersection
  (fromList $ abc !! 0) (fromList $ abc !! 1)) (fromList $ abc !! 2))

itemVal :: Char -> Int
itemVal a = if a <= 'Z' then 27 + ord(a) - ord('A') else 1 + ord(a) - ord('a')

main = do
  d <- readFile "data/03.txt"
  let l = chunksOf 3 . lines $ d
  print l
  let tot = foldl (\acc r -> acc + (itemVal . commonItem $ r)) 0 l
  print tot
