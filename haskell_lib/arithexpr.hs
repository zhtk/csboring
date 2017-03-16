data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2

policz :: Exp -> Int
policz e =
  let
    emptyStore x = undefined
    go :: Exp -> (String -> Int) -> Int
    go e st = case e of
      EInt x -> x
      EAdd e1 e2 -> (go e1 st)+(go e2 st)
      ESub e1 e2 -> (go e1 st)-(go e2 st)
      EMul e1 e2 -> (go e1 st)*(go e2 st)
      EVar z -> st z
      ELet var e1 e2 -> let 
        store name
         | name == var = go e1 st
         | otherwise = st name
        in go e2 store
  in go e emptyStore

instance Eq Exp where
  a == b = policz a == policz b

instance Show Exp where
  show e = case e of
    EInt x -> show x
    EAdd e1 e2 -> (show e1)++"+"++(show e2)
    ESub e1 e2 -> (show e1)++"-"++(show e2)
    EMul e1 e2 -> (show e1)++"*"++(show e2)
    EVar z -> z
    ELet var e1 e2 -> "let "++var++" = "++(show e1)++" in "++(show e2)

instance Num Exp where
  fromInteger x = EInt (fromInteger x)
  negate e = EMul e (EInt $ negate 1)
  (+) a b = EAdd a b
  (-) a b = ESub a b
  (*) a b = EMul a b
  abs e = EInt (abs $ policz e)
  signum e = EInt (signum $ policz e)

testExp1 :: Exp
testExp1 = (ELet "x" (EAdd (EInt 1) (EInt 2)) (EMul (EInt 2) (EVar "x")))

testExp2 :: Exp
testExp2 = (2 + 2) * 3

-- simpl :: Exp -> Exp
-- deriv :: String -> Exp -> Exp
