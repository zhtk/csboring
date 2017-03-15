import Prelude hiding(Either(..))
import Tree

data Either a b = Left a | Right b

instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left a) = Left a
  fmap f (Right a) = Right (f a)

reverseRight :: Either e [a] -> Either e [a]
-- Wersja wprost
--reverseRight (Left a) = Left a
--reverseRight (Right a) = Right (reverse a)
-- Wersja funktorowa
reverseRight = fmap reverse

class Functor f => Pointed f where
  pure :: a -> f a

-- ???
instance Pointed [a] where
  pure a = 

-- list, Maybe, Tree

