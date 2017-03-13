triads :: Int -> [(Int,Int,Int)]

triples n = [(a,b,c) | a <- [1..n], b <-[1..n], c<-[1..n] ]
pitag (a,b,c) = a*a + b*b == c*c
triads n = filter pitag $ triples n

prime n = null [p | p <- [2..n-1], mod n p == 0]
primes = [n | n <- [2..], prime n]

-- zadanie 7
--fib :: Eq, Num a => (a, a) -> a -> a
fib (a,b) n
  | n > 0 = fib (b, a+b) (n-1)
  | n == 0 = b
  | otherwise = undefined

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs [] where
  reverse'' [] acc = acc
  reverse'' (x:xs) acc = reverse'' xs (x:acc)

-- zadanie 8
indexOf :: Char -> String -> Maybe Int
indexOf c xs = indexOf' c xs 0 where
  indexOf' c [] pos = Nothing
  indexOf' c (x:xs) pos
    | x == c = Just pos
    | otherwise = indexOf' c xs (pos + 1)

positions :: Char -> String -> [Int]
positions c xs = reverse $ go c xs 0 [] where
  go c [] n acc = acc
  go c (x:xs) n acc
    | x == c = go c xs (n+1) (n:acc)
    | otherwise = go c xs (n+1) acc

-- zadanie 11
-- a
incAll :: [[Int]] -> [[Int]]
incAll xs = foldr fun [] xs where
  fun ns xs = (foldr fun2 [] ns):xs where
    fun2 n xs = (n+1):xs

-- b
silnia n = foldr (*) 1 [1..n]
concat' xs = foldr (++) [] xs

-- c
nub :: Eq a => [a] -> [a]
nub xs =
  let 
    f acc [] = acc
    f acc (x:xs) = f (acc++[x]) (filter (/=x) xs)
  in f [] xs
  

