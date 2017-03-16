import Prelude hiding(Either(..))
import Tree

data Either a b = Left a | Right b

-- Zadanie 2a
instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left a) = Left a
  fmap f (Right a) = Right (f a)

-- Zadanie 2b
reverseRight :: Either e [a] -> Either e [a]
-- Wersja wprost
--reverseRight (Left a) = Left a
--reverseRight (Right a) = Right (reverse a)
-- Wersja funktorowa
reverseRight = fmap reverse

-- Zadanie 2c
class Functor f => Pointed f where
  pure :: a -> f a

instance Pointed Tree where
  pure a = Node a Empty Empty

instance Pointed Maybe where
  pure a = Just a

instance Pointed [] where
  pure a = [a]

-- Usage:
-- pure 5 :: Maybe Int
