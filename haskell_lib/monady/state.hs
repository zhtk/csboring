{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumberTree :: Tree a -> Tree Int
renumberTree tree = let
    go :: (MonadState s m, Num s) => Tree t -> m (Tree s)
    go Empty = return Empty
    go (Node v l r) = do
      l' <- go l
      v <- get
      modify (+1)
      r' <- go r
      return (Node v l' r')
  in evalState (go tree) 1

--Stmt:   S ::= skip | x := e | S1;S2
--        | if b then S1 else S2 | while b do S

-- Wyrażenia boolowskie są trochę na wyrost
data Stmt = Skip
 | SLet String Int
 | SCol Stmt Stmt
 | SIf Bool Stmt Stmt
 | SWhile Bool Stmt

define :: String -> Int -> (String -> Int) -> (String -> Int)
define var val env et
 | var == et = val
 | otherwise = env et

execStmt' :: (MonadState (String -> Int) m) => Stmt -> m ()
execStmt' Skip = return ()
execStmt' (SLet var val) = do
  modify $ define var val
  return ()
execStmt' (SCol s1 s2) = do
  execStmt' s1
  execStmt' s2
  return ()
execStmt' (SIf cond s1 s2) = 
  if cond 
  then do
    execStmt' s1
    return ()
  else do
    execStmt' s2
    return ()
execStmt' (SWhile cond stmt) = 
  if cond 
  then do
    execStmt' $ SCol stmt (SWhile cond stmt)
    return ()
  else
    return ()

-- wynik trzymamy w zmiennej x
execStmt :: Stmt -> Int
execStmt stmt = execState (execStmt' stmt) undefined "x"

testStmt = SCol (SLet "x" 10) (SLet "x" 42)
