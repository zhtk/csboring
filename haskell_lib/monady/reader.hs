import Control.Monad.Reader

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

-- Wersja bez monad
renumber :: Tree a -> Tree Int
renumber tree = let
  go t n = case t of
    Empty -> Empty
    Node _ l r -> Node n (go l (n+1)) (go r (n+1))
  in go tree 1

-- Wersja monadyczna
renumberM :: Tree a -> Reader Int (Tree Int)
renumberM Empty = return Empty
renumberM (Node _ l r) = do
  l' <- local (+1) $ renumberM l
  r' <- local (+1) $ renumberM r
  g <- ask
  return $ Node g l' r'

renumber' :: Tree a -> Tree Int
renumber' tree = runReader (renumberM tree) 1

-- Punkt B zadania
type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

type Env = Var -> Int

evalExp :: Exp -> Int
evalExp exp = let
  define :: Var -> Int -> Env -> Env
  define var e f et
   | et == var = e
   | otherwise = f et
  go :: Exp -> Reader Env Int
  go e = case e of
    EInt num -> return num
    EOp OpAdd e1 e2 -> do
      v1 <- go e1
      v2 <- go e2
      return $ v1 + v2
    EOp OpMul e1 e2 -> do
      v1 <- go e1
      v2 <- go e2
      return $ v1 * v2
    EOp OpSub e1 e2 -> do
      v1 <- go e1
      v2 <- go e2
      return $ v1 - v2
    EVar var -> do
      env <- ask
      return (env var)
    ELet var e1 e2 -> do
      val <- go e1
      local (define var val) (go e2)
  in runReader (go exp) undefined

--
--      let x =
--          let y = 6 + 9
--          in y - 1
--      in x * 3
-- 
-- ==>  42
--
test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9)) (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"
