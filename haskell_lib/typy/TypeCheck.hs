module TypeCheck where

import IntLambda
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Env = Map.Map Name Type

typeOf :: Exp -> Type
typeOf = go Map.empty where
  go :: Env -> Exp -> Type
  go _ (EInt _) = TInt
  go d (ELam n t e) = t :-> t' where
    d' = Map.insert n t d
    t' = go d' e
  go d (EVar n) = fromJust $ Map.lookup n d
  go d (EApp e e') = case go d e of
    t' :-> t | t' == go d e' -> t
  
