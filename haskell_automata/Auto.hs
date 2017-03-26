module Auto(Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
import Data.List

data Auto a q = A { 
  states :: [q]
  , initStates  :: [q]
  , isAccepting :: q -> Bool
  , transition  :: q -> a -> [q]
}

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts (A st init acc tr) w = 
  let
    next qs a = nub $ concat $ map (\q -> tr q a) qs
    f xs w = case (xs, w) of
      ([], _) -> False
      (_, []) -> any acc xs
      (_:_, w:ws) -> f (next xs w) ws
  in f (nub init) w

-- Funkcja reprezentująca brak przejść
noTransition :: q -> a -> [q]
noTransition q a = []

emptyA :: Auto a ()
emptyA = A [()] [] (\x -> False) noTransition

epsA :: Auto a ()
epsA = A [()] [()] (\x -> True) noTransition

-- Funkcja reprezentująca dokładnie jedno przejście
symTransition :: Eq a => a -> Bool -> a -> [Bool]
symTransition z q a
  | z == a && q == False = [True]
  | otherwise = []

symA :: Eq a => a -> Auto a Bool
symA c = A [True, False] [False] (\q -> q) (symTransition c)

leftA :: Auto a q -> Auto a (Either q r)
leftA (A st init acc tr) =
  let
    f x = Left x
    acc' q = case q of
      Left x  -> acc x
      Right x -> False
    tr' q z = case q of
      Left x  -> map f (tr x z)
      Right x -> []
  in A (map f st) (map f init) acc' tr'

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA (A st1 init1 acc1 tr1) (A st2 init2 acc2 tr2) =
  let
    st = (map Left st1)++(map Right st2)
    init = (map Left init1)++(map Right init2)
    acc = either acc1 acc2
    tr q z = case q of
      Left x -> map Left (tr1 x z)
      Right x -> map Right (tr2 x z)
  in A st init acc tr

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA (A st1 init1 acc1 tr1) (A st2 init2 acc2 tr2) =
  let
    st = (map Left st1)++(map Right st2)
    init = (map Left init1)++(map Right init2)
    acc1' q = acc1 q && any acc2 init2
    acc = either acc1' acc2
    tr (Left q) z =
      if any acc1 (tr1 q z) then (map Left (tr1 q z))++(map Right init2)
      else map Left (tr1 q z)
    tr (Right q) z = map Right (tr2 q z)
  in A st init acc tr

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists st init acc tr =
  let
    acc' q = any (q==) acc
    filtr q a = filter (\(x,y,_) -> x == q && y == a) tr
    tr' q a = concat $ map (\(_,_,x) -> x) (filtr q a)
  in A st init acc' tr'

toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists (A st init acc tr) =
  let
    acc' = filter acc st
    alpha = [minBound..maxBound]
    tr' = [(s,a,tr s a) | s <- st, a <- alpha]
  in (st, init, acc', tr')

