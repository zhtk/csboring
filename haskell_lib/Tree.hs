module Tree where

-- Zadanie 1
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

-- a
instance Show a => Show (Tree a) where
   show t = case t of
      Empty -> "(*)"
      Node v l r -> 
        unwords ["(", (show v), "->", (show l), "+", (show r), ")"]

instance Eq a => Eq (Tree a) where
   t1 == t2 = case (t1, t2) of
     (Empty, Empty) -> True
     ((Node v1 l1 r1),(Node v2 l2 r2)) ->
       v1 == v2 && l1 == l2 && r1 == r2
     (_,_) -> False

-- b
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f l)


-- c
toList :: Tree a -> [a]
toList Empty = []
toList (Node v l r) = (toList l)++[v]++(toList r)


-- d
insert :: (Ord a) => a -> Tree a -> Tree a
insert val Empty = Node val Empty Empty
insert val (Node v l r)
 | val < v = Node v (insert val l) r
 | otherwise = Node v l (insert val r)

contains :: (Ord a) => a -> Tree a -> Bool
contains a Empty = False
contains a (Node v l r)
 | a == v = True
 | a < v = contains a l
 | otherwise = contains a r


fromList :: (Ord a) => [a] -> Tree a
fromList xs = foldr insert Empty xs

-- e
treesort :: Ord a => [a] -> [a]
treesort xs = toList $ fromList xs

