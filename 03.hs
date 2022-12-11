import Data.Char (ord)
import Data.Set (fromList, intersection, elemAt)

rucksackHalves :: [Char] -> ([Char], [Char])
rucksackHalves sack = (
  take (div (length sack) 2) sack,
  drop (div (length sack) 2) sack)

commonItem :: [Char] -> [Char] -> Char
commonItem a b = elemAt 0 (intersection (fromList a) (fromList b))

itemVal :: Char -> Int
itemVal a = if a <= 'Z' then 27 + ord(a) - ord('A') else 1 + ord(a) - ord('a')

main = do
  d <- readFile "data/03.txt"
  let l = lines d
  let tot = foldl (\acc r -> acc + (itemVal . uncurry commonItem . rucksackHalves $ r)) 0 l
  print tot
