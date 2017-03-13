
szescian x = x*x*x

absolut x | x >= 0 =  x
          | x < 0  = -x

hed (x:xs) = x
til (x:xs) = xs
--til [x] = x
--til (x:xs) = til xs

sklej [] xs = xs
sklej ys xs = sklej (init ys) ((last ys):xs)

take2 acc n (l:ls) = if n == 0 then acc else take2 (acc ++ l) (n-1) ls
tke = take2 []

drp 0 l = l
drp n (l:ls) = drp (n-1) ls

-- acc = wynik
-- tmp = początek listy
-- l   = pozostała część listy do doklejenia
inits2 :: [[a]] -> [a] -> [a] -> [[a]]
inits2 acc tmp [] = acc++[tmp]
inits2 acc tmp (l:ls) = inits2 (acc++[tmp]) (tmp++[l]) ls

ints :: [a] -> [[a]]
ints l = inits2 [] [] l

partitions :: [a] -> [([a],[a])]
partitions xs = [ (take n xs, drop n xs) | n <- [0..length xs] ]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations l = [ [head $ drop (n-1) l]++r | n <- [1..length l], r <- permutations $ (take (n-1) l)++(drop n l)]

nub :: Eq a => [a] -> [a]
nub [] = []
nub (l:ls) = 
  let 
    fix n ls = case ls of
      h:t | h == n -> fix n t
      h:t -> h:(fix n t)
      [] -> [] 
  in l:(nub $ fix l ls)
