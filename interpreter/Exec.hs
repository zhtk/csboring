module Exec where

import System.IO.Unsafe
import Text.Read (readMaybe)

import Abslang
import ErrM
import qualified Data.Map as Map

putVarToEnv :: String -> Stored -> InterState -> Err InterState
putVarToEnv name val (env, store, alloc) =
  return (Map.insert name alloc env, Map.insert alloc val store, alloc + 1)

putArgsToEnv :: [Decl] -> [Stored] -> InterState -> Err InterState
putArgsToEnv decl vals (env, store, alloc) = case (decl, vals) of
  ([], []) -> return (env, store, alloc)
  (d:ds, v:vs) -> do
    let Decl _ (Ident id) = d
    inter <- putVarToEnv id v (env, store, alloc)
    putArgsToEnv ds vs inter
  (_, _) -> fail $ "Runtime: putArgsToEnv - length mismatch"

runInstructions :: InterState -> Env -> [Inst] -> Err (Maybe Stored, Store)
runInstructions (env, store, alloc) global instr = case instr of
  [] -> return (Nothing, store)
  (i:is) -> do
    res <- transInst (env, store, alloc) global i
    case res of
      (Just x, (_, store, _)) -> return (Just x, store)
      (Nothing, inter) -> runInstructions inter global is

runFunction :: InterState -> String -> [Stored] -> Err (Stored, Store)
runFunction (env, store, alloc) name args = do
  -- Extract function from env
  let Just (SFun fun) = (Map.lookup name env) >>= (\l -> Map.lookup l store)
  let (Fun tp _ decls insts) = fun
  -- Insert arguments to env
  (env', store, alloc) <- putArgsToEnv decls args (env, store, alloc)
  -- Execute instructions
  (result, store) <- runInstructions (env', store, alloc) env insts
  -- Return default value
  case result of
    Just res -> return (res, store)
    Nothing -> return $ (getDefaultValue tp, store)

getDefaultValue :: Type -> Stored
getDefaultValue x = case x of
  TInt -> SInt 0
  TBool -> SBool False
  TStr -> SStr ""
  TTab tp n -> STab tp n $ foldl (\m i -> Map.insert i (getDefaultValue tp) m) Map.empty [0..(n-1)]
  TDict tst tind -> SDict tst tind Map.empty

parseArgs :: InterState -> Env -> [Exp] -> Err ([Stored], InterState)
parseArgs inter global [] = return ([], inter)
parseArgs inter global (e:es) = do
  (arg, inter) <- transExp inter global e
  (stores, inter) <- parseArgs inter global es
  return ((arg:stores), inter)

strToInt :: [Stored] -> Err Stored
strToInt args = do
  s <- case args of
    [SStr s] -> return s
    _ -> fail "Runtime: static check error in toint function"
  let val = readMaybe s
  case val of
    Just x -> return $ SInt x
    Nothing -> fail $ "Execution error: toint can't convert " ++ s

intToStr :: [Stored] -> Err Stored
intToStr args = do
  i <- case args of
    [SInt i] -> return i
    _ -> fail "Runtime: static check error in toint function"
  return $ SStr $ show i


transExp :: InterState -> Env -> Exp -> Err (Stored, InterState)
transExp (env, store, alloc) global x = case x of
  EAnd exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SBool a, SBool b) -> return (SBool (a && b), inter)
      _ -> fail "Runtime: static check error in EAnd"
  EOr exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SBool a, SBool b) -> return (SBool (a || b), inter)
      _ -> fail "Runtime: static check error in EOr"
  EGte exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a >= b), inter)
      _ -> fail "Runtime: static check error in EGte"
  EGt exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a > b), inter)
      _ -> fail "Runtime: static check error in EGt"
  ELte exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a <= b), inter)
      _ -> fail "Runtime: static check error in ELte"
  ELt exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a < b), inter)
      _ -> fail "Runtime: static check error in ELt"
  ENeq exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a /= b), inter)
      (SStr a, SStr b) -> return (SBool (a /= b), inter)
      (SBool a, SBool b) -> return (SBool (a /= b), inter)
      _ -> fail "Runtime: static check error in ENeq"
  EEq exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SBool (a == b), inter)
      (SStr a, SStr b) -> return (SBool (a == b), inter)
      (SBool a, SBool b) -> return (SBool (a == b), inter)
      _ -> fail "Runtime: static check error in EEq"
  EAdd exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SInt (a + b), inter)
      (SStr a, SStr b) -> return (SStr (a ++ b), inter)
      _ -> fail "Runtime: static check error in EAdd"
  ESub exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SInt (a - b), inter)
      _ -> fail "Runtime: static check error in ESub"
  EMul exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt a, SInt b) -> return (SInt (a * b), inter)
      _ -> fail "Runtime: static check error in EMul"
  EDiv exp0 exp -> do
    (e1, inter) <- transExp (env, store, alloc) global exp0
    (e2, inter) <- transExp inter global exp
    case (e1, e2) of
      (SInt _, SInt 0) -> fail "Runtime error: division by 0!"
      (SInt a, SInt b) -> return (SInt (a `div` b), inter)
      _ -> fail "Runtime: static check error in EDiv"
  ENeg exp -> do
    (exp, inter) <- transExp (env, store, alloc) global exp
    case exp of
      SInt n -> return (SInt (-n), inter)
      _ -> fail "Runtime: static check error in ENeg"
  Call (Ident id) exps -> do
    let fun = (Map.lookup id env) >>= (\l -> Map.lookup l store)
    (args, (env, store, alloc)) <- parseArgs (env, store, alloc) global exps
    case (fun, id) of
      (Just _, _) -> do
        (res, store) <- runFunction (env, store, alloc) id args
        return (res, (env, store, alloc))
      (Nothing, "toint") -> do
        res <- strToInt args
        return (res, (env, store, alloc))
      (Nothing, "tostr") -> do
        res <- intToStr args
        return (res, (env, store, alloc))
      _ -> fail $ "Runtime: static check error, no such function: " ++ id
  EVar (Ident id) -> do
    let Just val = (Map.lookup id env) >>= (\l -> Map.lookup l store)
    return (val, (env, store, alloc))
  EInt n -> return (SInt n, (env, store, alloc))
  ETab (Ident id) exp -> do
    (val, inter) <- transExp (env, store, alloc) global exp
    tab <- case (Map.lookup id env) >>= (\l -> Map.lookup l store) of
      Nothing -> fail $ "Runtime: static check error, no variable " ++ id
      Just x -> return x
    val <- case tab of
      STab _ n map -> case val of
        SInt x | x >= 0 && x < n -> do
          let Just r = Map.lookup x map
          return r
        SInt x -> fail $ "Execution error: index of " ++ id ++ " out of range. Index value: " ++ (show x)
        _ -> fail "Runtime: static checking error, index is not an SInt"
      SDict _ _ map -> case Map.lookup val map of
        Just x -> return x
        Nothing -> fail $ "Execution error: key " ++ (getPrettyPrint val) ++ " not in dictionary " ++ id 
      _ -> fail $ "Runtime: static checking error, variable " ++ id ++ " is not tab/dict"
    return (val, (env, store, alloc))
  EStr str -> return (SStr str, (env, store, alloc))
  ETrue -> return (SBool True, (env, store, alloc))
  EFalse -> return (SBool False, (env, store, alloc))

prettyPrintTab :: Map.Map Integer Stored -> Integer -> Integer -> String -> String
prettyPrintTab tab i n s
  | i >= n = s
  | i == 0 = let Just val = Map.lookup i tab
    in prettyPrintTab tab (i+1) n $ s ++ (getPrettyPrint val)
  | otherwise = let Just val = Map.lookup i tab
    in prettyPrintTab tab (i+1) n $ s ++ ", " ++ (getPrettyPrint val)

prettyPrintDict :: [(Stored, Stored)] -> String
prettyPrintDict [] = "}"
prettyPrintDict [d] = (getPrettyPrint $ fst d) ++ " -> " ++ (getPrettyPrint $ snd d) ++ "}"
prettyPrintDict (d:ds) = (getPrettyPrint $ fst d) ++ " -> " ++ (getPrettyPrint $ snd d) ++ "; " ++ (prettyPrintDict ds)

getPrettyPrint :: Stored -> String
getPrettyPrint val = case val of
  SInt i -> show i
  SStr s -> s
  SBool b -> show b
  STab _ n tab -> "[" ++ (prettyPrintTab tab 0 n "") ++ "]"
  SDict _ _ dict -> "{" ++ prettyPrintDict (Map.toList dict)
  SFun (Fun _ (Ident id) _ _) -> "Function " ++ id

transInst :: InterState -> Env -> Inst -> Err (Maybe Stored, InterState)
transInst (env, store, alloc) global x = case x of
  IVar tp (Ident id) -> do
    let val = getDefaultValue tp
    return (Nothing, (Map.insert id alloc env, Map.insert alloc val store, alloc + 1))
  ISet (Ident id) exp -> do
    (val, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    let Just loc = Map.lookup id env
    return (Nothing, (env, Map.insert loc val store, alloc))
  ITSet (Ident id) exp0 exp -> do
    (e1, (env, store, alloc)) <- transExp (env, store, alloc) global exp0
    (e2, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    let Just loc = Map.lookup id env
    tab <- case Map.lookup loc store of
      Nothing -> fail "Runtime: dict/tab variable not fund"
      Just x -> return x
    tab <- case tab of
      STab t n map -> do
        ind <- case e1 of
          SInt x | x >= 0 && x < n -> return x
          SInt x -> fail $ "Execution error: index of " ++ id ++ " out of range. Index value: " ++ (show x)
          _ -> fail "Runtime: static checking error, index is not an SInt"
        return $ STab t n (Map.insert ind e2 map)
      SDict t1 t2 map -> return $ SDict t1 t2 (Map.insert e1 e2 map)
      _ -> fail "Runtime: static checking error, variable is not tab/dict"
    return (Nothing, (env, Map.insert loc tab store, alloc))
  IIf exp insts -> do
    (val, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    case val of
      SBool True -> do
        (ret, store) <- runInstructions (env, store, alloc) global insts
        return (ret, (env, store, alloc))
      SBool False -> return (Nothing, (env, store, alloc))
      _ -> fail "Runtime: static check error in IF"
  IIfe exp insts0 insts -> do
    (val, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    case val of
      SBool True -> do
        (ret, store) <- runInstructions (env, store, alloc) global insts0
        return (ret, (env, store, alloc))
      SBool False -> do
        (ret, store) <- runInstructions (env, store, alloc) global insts
        return (ret, (env, store, alloc))
      _ -> fail "Runtime: static check error in IF"
  IWhile exp insts -> do
    (val, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    case val of
      SBool False -> return (Nothing, (env, store, alloc))
      SBool True -> do
        (ret, store) <- runInstructions (env, store, alloc) global insts
        (ret, store) <- case ret of
          Just _ -> return (ret, store)
          Nothing -> runInstructions (env, store, alloc) global [IWhile exp insts]
        return (ret, (env, store, alloc))
      _ -> fail "Runtime: static check error in WHILE"
  ICall (Ident id) exps -> do
    let fun = (Map.lookup id env) >>= (\l -> Map.lookup l store)
    (args, (env, store, alloc)) <- parseArgs (env, store, alloc) global exps
    case (fun, id) of
      (Just _, _) -> do
        (res, store) <- runFunction (env, store, alloc) id args
        return (Nothing, (env, store, alloc))
      (Nothing, "toint") -> do
        res <- strToInt args
        return (Nothing, (env, store, alloc))
      (Nothing, "tostr") -> do
        res <- intToStr args
        return (Nothing, (env, store, alloc))
      _ -> fail $ "Runtime: static check error, no such function: " ++ id
  IPrint exp -> do
    (val, inter) <- transExp (env, store, alloc) global exp
    seq (unsafePerformIO $ putStrLn $ getPrettyPrint val) $ return ()
    return (Nothing, inter)
  IFort id exp0 exp insts -> do
    (beg, (env, store, alloc)) <- transExp (env, store, alloc) global exp0
    (end, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    case (beg, end) of
      (SInt a, SInt b) | a > b -> return (Nothing, (env, store, alloc))
      (SInt a, SInt b) | a <= b -> do
        (env', store', alloc') <- putArgsToEnv [Decl TInt id] [SInt a] (env, store, alloc)
        (ret, store) <- runInstructions (env', store', alloc') global insts
        (ret, store) <- case ret of
          Just _ -> return (ret, store)
          Nothing -> runInstructions (env, store, alloc) global [IFort id (EInt $ a + 1) (EInt b) insts]
        return (ret, (env, store, alloc))
      _ -> fail "Runtime: static check error in WHILE"
  IFord id exp0 exp insts -> do
    (beg, (env, store, alloc)) <- transExp (env, store, alloc) global exp0
    (end, (env, store, alloc)) <- transExp (env, store, alloc) global exp
    case (beg, end) of
      (SInt a, SInt b) | a < b -> return (Nothing, (env, store, alloc))
      (SInt a, SInt b) | a >= b -> do
        (env', store', alloc') <- putArgsToEnv [Decl TInt id] [SInt a] (env, store, alloc)
        (ret, store) <- runInstructions (env', store', alloc') global insts
        (ret, store) <- case ret of
          Just _ -> return (ret, store)
          Nothing -> runInstructions (env, store, alloc) global [IFord id (EInt $ a - 1) (EInt b) insts]
        return (ret, (env, store, alloc))
      _ -> fail "Runtime: static check error in WHILE"
  IBlock insts -> do
    (ret, store) <- runInstructions (env, store, alloc) global insts
    return (ret, (env, store, alloc))
  IRet exp -> do
    (val, inter) <- transExp (env, store, alloc) global exp
    return (Just val, inter)
