sequence' :: Monad m => [m a] -> m [a] 
sequence' [] = return []
sequence' (x:xs) = do
  m <- x;
  ms <- sequence' xs;
  return (m:ms)

-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- liftM2 f x y = ...
-- sequence' xs = foldr (liftM2 (:)) (return []) xs

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f xs = sequence $ map f xs

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' = flip mapM'

