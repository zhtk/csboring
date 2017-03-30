allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = do
  x <- xs
  y <- ys
  return [x,y]

allCombinations :: [[a]] -> [[a]]
allCombinations [] = return []
allCombinations (l:ls) = do
  x <- l
  y <- allCombinations ls
  return (x:y)

