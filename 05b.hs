import Data.List.Split (splitOn)

readInt :: String -> Int
readInt = read

parseData :: [String] -> IO ([String], [[Int]])
parseData l = do
  let stacksInstructions = splitOn [""] l
  return (parseStacks $ stacksInstructions !! 0,
    parseInstructions $ stacksInstructions !! 1)

parseStacks :: [String] -> [String]
parseStacks l = foldl parseStackRow (take (div (length (last l)+1) 4) . repeat $ "") . init $ l

parseStackRow :: [String] -> String -> [String]
parseStackRow stacks r = foldl (\stacks (v, index) -> if mod index 4 /= 1 || v == ' ' then stacks else
    (take (div index 4) stacks) ++ [(stacks !! div index 4) ++ [v]] ++ (drop ((div index 4)+1) stacks)
    ) stacks (zip r [0..])

parseInstructions :: [String] -> [[Int]]
parseInstructions l = map (\i -> [readInt $ (words i) !! 1, (readInt $ (words i) !! 3) - 1, (readInt $ (words i) !! 5) - 1]) l

runInstructions :: ([String], [[Int]]) -> [String]
runInstructions (stacks, instructions) = foldl runInstruction stacks instructions

runInstruction :: [String] -> [Int] -> [String]
runInstruction stacks i = do
  let crates = take (i !! 0) (stacks !! (i !! 1))
  let ns = (take (i !! 1) stacks) ++ [drop (i !! 0) (stacks !! (i !! 1))] ++ (drop ((i !! 1)+1) stacks)
  (take (i !! 2) ns) ++ [crates ++ ns !! (i !! 2)] ++ (drop ((i !! 2)+1) ns)

main = do
  d <- readFile "data/05.txt"
  let l = lines d
  x <- parseData l
  let o = runInstructions x
  print (map head o)