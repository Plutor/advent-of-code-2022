import Data.Set (Set)
import qualified Data.Set as Set

readInt :: String -> Int
readInt = read

type Path = [String]
data File = File {
  fname :: String,
  size :: Int
} deriving (Ord, Eq)
data Dir = Dir {
  dname :: String,
  totsize :: Int,
  files :: Set File,
  dirs :: Set Dir
} deriving (Ord, Eq)

addDir :: Dir -> Path -> String -> Dir
addDir d [] n = Dir{ dname=dname d, totsize=totsize d, files=files d,
                     dirs=Set.insert Dir{dname=n, totsize=0, files=Set.empty, dirs=Set.empty} (dirs d)}
addDir d (p:ps) n = Dir{ dname=dname d, totsize=totsize d, files=files d,
                         dirs=Set.insert (addDir (getDir (dirs d) p) ps n) (Set.delete (getDir (dirs d) p) (dirs d)) }

addFile :: Dir -> Path -> String -> Int -> Dir
addFile d [] n s = Dir{ dname=dname d, dirs=dirs d, totsize=(totsize d)+s,
                        files=Set.insert File{fname=n, size=s} (files d)}
addFile d (p:ps) n s = Dir{ dname=dname d, files=files d, totsize=(totsize d)+s,
                            dirs=Set.insert (addFile (getDir (dirs d) p) ps n s) (Set.delete (getDir (dirs d) p) (dirs d)) }

getDir :: Set Dir -> String -> Dir
getDir s n = Set.findMin (Set.filter (\x -> dname x == n) s)

parseLine :: (Dir, Path) -> String -> (Dir, Path)
parseLine (root,pwd) line = case words line of
                          ["$", "cd", ".."] -> (root, init pwd)
                          ["$", "cd", n] -> (root, pwd ++ [n])
                          ["$", "ls"] -> (root, pwd)
                          ["dir", n] -> (addDir root pwd n, pwd)
                          [s, n] -> (addFile root pwd n (readInt s), pwd)

sumTotalsLT :: Int -> Dir -> Int
sumTotalsLT mx d = if totsize d < mx
                   then (totsize d) + (sum (Set.map (sumTotalsLT mx) (dirs d)))
                   else sum (Set.map (sumTotalsLT mx) (dirs d))

smallestDirGE :: Int -> Dir -> Int
smallestDirGE mx d = if totsize d < mx then maxBound
  else minimum ([totsize d] ++ map (smallestDirGE mx) (Set.toList . dirs $ d))

main = do
  d <- readFile "data/07.txt"
  let l = drop 1 . lines $ d
  let root = foldl parseLine (Dir{dname=[], totsize=0, files=Set.empty, dirs=Set.empty}, []) l
  print (sumTotalsLT 100000 (fst root))
  let needed = totsize (fst root) - 40000000
  print (smallestDirGE needed (fst root))
