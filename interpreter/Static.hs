module Static where

import Abslang
import ErrM
import Data.Map

-- entry point for static check
staticCheck :: InterState -> [Function] -> Err ()
staticCheck _ [] = return ()
staticCheck state (f:fs) =
  (checkMain state) >> (transFunction state f) >> (staticCheck state fs)

-- checks correctness of main function
checkMain :: InterState -> Err ()
checkMain (env, store, alloc) =
  case Data.Map.lookup "main" env of
    Just l -> case Data.Map.lookup l store of
      Just (SFun (Fun _ (Ident "main") [] _)) -> return ()
      Just (SFun (Fun _ _ _ _)) -> fail "Error: main should not take arguments"
      Just _ -> fail "Error: main is not a function"
      Nothing -> fail "Error: main definition not found in store"
    Nothing -> fail "Error: main is not specified"

-- gets type of variable
variableType :: String -> InterState -> Err Type
variableType str (env, store, alloc) =
  case Data.Map.lookup str env of
    Nothing -> fail $ "Undefined variable: " ++ str
    Just l -> case Data.Map.lookup l store of
      Just (SInt _) -> return TInt
      Just (SBool _) -> return TBool
      Just (SStr _) -> return TStr
      Just (STab t n _) -> return $ TTab t n
      Just (SDict t1 t2 _) -> return $ TDict t1 t2
      Just (SFun _) -> fail $ "Type error: expected variable, but " ++ str ++ " is a function"
      Nothing -> fail $ "Internal error: location of " ++ str ++ " not found"

-- embeds function arguments to the state
parseDecls :: [Decl] -> (Type, InterState) -> Err (Type, InterState)
parseDecls decls (ret, interstate) = let
  f (Decl t id) = IVar t id
  in transInstrs (Prelude.map f decls) (ret, interstate)

-- checks typing in function
transFunction :: InterState -> Function -> Err ()
transFunction interstate (Fun ret (Ident name) decls insts) = let
  parsedDecls = parseDecls decls (ret, interstate)
  in case parsedDecls >>= (transInstrs insts) of
    Ok _ -> return ()
    Bad s -> fail $ s ++ "\nIn function: " ++ name

-- gets type of expression
transExp :: Exp -> InterState -> Err Type
transExp x inter = case x of
  EAnd exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TBool, TBool) -> return TBool
      _ -> fail $ "Error: TBools expected in " ++ (show t1) ++ " && " ++ (show t2)
  EOr exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TBool, TBool) -> return TBool
      _ -> fail $ "Error: TBools expected in " ++ (show t1) ++ " || " ++ (show t2)
  EGte exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      _ -> fail $ "Error: TInt expected in " ++ (show t1) ++ " >= " ++ (show t2)
  EGt exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      _ -> fail $ "Error: TInt expected in " ++ (show t1) ++ " > " ++ (show t2)
  ELte exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      _ -> fail $ "Error: TInt expected in " ++ (show t1) ++ " <= " ++ (show t2)
  ELt exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      _ -> fail $ "Error: TInt expected in " ++ (show t1) ++ " < " ++ (show t2)
  ENeq exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      (TStr, TStr) -> return TBool
      (TBool, TBool) -> return TBool
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " != " ++ (show t2)
  EEq exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TBool
      (TStr, TStr) -> return TBool
      (TBool, TBool) -> return TBool
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " == " ++ (show t2)
  EAdd exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TInt
      (TStr, TStr) -> return TStr
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " + " ++ (show t2)
  ESub exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TInt
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " - " ++ (show t2)
  EMul exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TInt
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " * " ++ (show t2)
  EDiv exp0 exp -> do
    t1 <- transExp exp0 inter
    t2 <- transExp exp inter
    case (t1, t2) of
      (TInt, TInt) -> return TInt
      _ -> fail $ "Error: type mismatch in " ++ (show t1) ++ " / " ++ (show t2)
  ENeg exp -> do
    t <- transExp exp inter
    case t of
      TInt -> return TInt
      _ -> fail $ "Error: operator (0-) expected TInt, got " ++ (show t)
  Call (Ident id) exps -> do
    t <- checkCall id inter exps
    return t
  EVar (Ident id) -> variableType id inter
  EInt n -> return TInt
  ETab (Ident id) exp -> do
    indice <- transExp exp inter
    let (env, store, _) = inter
    case (Data.Map.lookup id env) >>= (\l -> Data.Map.lookup l store) of
      Just (STab t _ _) | indice == TInt -> return t
      Just (STab _ _ _) -> fail $ "Error: " ++ id ++ " table index expected (TInt), got " ++ (show indice)
      Just (SDict r t _) | indice == t -> return r
      Just (SDict _ t _) -> fail $ "Error: " ++ id ++ " dictionary key expected " ++ (show t) ++ ", got " ++ (show indice)
      Just _ -> fail $ "Error: expected that " ++ id ++ " is a table or dict"
      Nothing -> fail $ "Error: undefined table/dict " ++ id
  EStr str -> return TStr
  ETrue -> return TBool
  EFalse -> return TBool

-- checks correctness of sequence of instructions
transInstrs :: [Inst] -> (Type, InterState) -> Err (Type, InterState)
transInstrs [] environ = return environ
transInstrs (i:is) environ = (transInst i environ) >>= (transInstrs is)

-- converts type to stored value
toStored :: Type -> Stored
toStored t = case t of
  TInt -> SInt 0
  TBool -> SBool False
  TStr -> SStr ""
  TTab t size -> STab t size empty
  TDict t1 t2 -> SDict t1 t2 empty

checkTypesList :: [Exp] -> [Decl] -> InterState -> Err ()
checkTypesList exps types inter =
  case (exps, types) of
    (e:es, t:ts) -> do
      let Decl tt (Ident arg) = t
      et <- transExp e inter
      if et == tt then checkTypesList es ts inter
      else fail $ "Error: argument " ++ arg ++ " type mismatch, expected " ++ (show tt) ++ " got " ++ (show et)
    ([], []) -> return ()
    ([], _:_) -> fail $ "Error: Too small number of passed arguments"
    (_:_, []) -> fail $ "Error: Too many passed arguments"

-- checks type of function call
checkCall :: String -> InterState -> [Exp] -> Err Type
checkCall id (env, store, alloc) exps = do
  fun <- case (Data.Map.lookup id env) >>= (\l -> Data.Map.lookup l store) of
    Just (SFun f) -> return f
    Just _ -> fail $ "Error: " ++ id ++ " is not a function"
    Nothing | id == "tostr" -> return (Fun TStr (Ident "tostr") [Decl TInt (Ident "i")] [])
    Nothing | id == "toint" -> return (Fun TInt (Ident "toint") [Decl TStr (Ident "s")] [])
    Nothing -> fail $ "Error: unknown function " ++ id
  let (Fun ret _ args _) = fun
  let types (Decl t _) = t
  case checkTypesList exps args (env, store, alloc) of
    Ok () -> return ret
    Bad s -> fail $ s ++ "\nIn " ++ id ++ " function call"

-- checks typing in instructions
transInst :: Inst -> (Type, InterState) -> Err (Type, InterState)
transInst x inter@(funType, (env, store, alloc)) = case x of
  IVar type' (Ident id) -> do
    let env' = insert id alloc env
    let val' = toStored type'
    let store' = insert alloc val' store
    return (funType, (env', store', alloc + 1))
  ISet (Ident id) exp -> do
    vtype <- variableType id (env, store, alloc)
    et <- transExp exp (env, store, alloc)
    if et == vtype then return inter
    else fail $ "Error: variable " ++ id ++ " holds values of type " ++ (show vtype) ++ ", but got " ++ (show et)
  ITSet (Ident id) exp0 exp -> do
    vtype <- variableType id (env, store, alloc)
    et1 <- transExp exp0 (env, store, alloc)
    et2 <- transExp exp (env, store, alloc)
    case vtype of
      TTab tt1 _ | tt1 /= et2 -> fail $ "Error: table " ++ id ++ " holds " ++ (show tt1) ++ ", got " ++ (show et2)
      TTab _ _ | TInt /= et1 -> fail $ "Error: table " ++ id ++ " expects TInt index, got " ++ (show et1)
      TTab _ _ -> return ()
      TDict _ dt | dt /= et1 -> fail $ "Error: dictionary " ++ id ++ " is indexed with "++ (show dt) ++", got " ++ (show et1)
      TDict dt _ | dt /= et2 -> fail $ "Error: dictionary " ++ id ++ " holds "++ (show dt) ++" values, got " ++ (show et1)
      TDict _ _ -> return ()
      _ -> fail $ "Error: " ++ id ++ " is not a table or dictionary"
    return inter
  IIf exp insts -> do
    cond <- transExp exp (env, store, alloc)
    case cond of
      TBool -> return ()
      _ -> fail $ "Error: IF condition must be of type TBool, got " ++ (show cond)
    transInstrs insts inter
    return inter
  IIfe exp insts0 insts -> do
    cond <- transExp exp (env, store, alloc)
    case cond of
      TBool -> return ()
      _ -> fail $ "Error: IF condition must be of type TBool, got " ++ (show cond)
    transInstrs insts0 inter
    transInstrs insts inter
    return inter
  IWhile exp insts -> do
    cond <- transExp exp (env, store, alloc)
    case cond of
      TBool -> return ()
      _ -> fail $ "Error: WHILE condition must be of type TBool, got " ++ (show cond)
    transInstrs insts inter
    return inter
  ICall (Ident id) exps -> do
    checkCall id (env, store, alloc) exps
    return inter
  IPrint exp -> do
    transExp exp (env, store, alloc)
    return inter
  IFort (Ident id) exp0 exp insts -> do
    t1 <- transExp exp0 (env, store, alloc)
    t2 <- transExp exp (env, store, alloc)
    case t1 of
      TInt -> return ()
      _ -> fail $ "Error: FOR iterator " ++ id ++ " should be TInt, but is " ++ (show t1)
    case t2 of
      TInt -> return ()
      _ -> fail $ "Error: FOR iterator " ++ id ++ " should be TInt, but is " ++ (show t2)
    
    let env' = insert id alloc env
    let val' = toStored TInt
    let store' = insert alloc val' store
    transInstrs insts (funType, (env', store', alloc + 1))
    
    return inter
  IFord (Ident id) exp0 exp insts -> do
    t1 <- transExp exp0 (env, store, alloc)
    t2 <- transExp exp (env, store, alloc)
    case t1 of
      TInt -> return ()
      _ -> fail $ "Error: FOR iterator " ++ id ++ " should be TInt, but is " ++ (show t1)
    case t2 of
      TInt -> return ()
      _ -> fail $ "Error: FOR iterator " ++ id ++ " should be TInt, but is " ++ (show t2)
    
    let env' = insert id alloc env
    let val' = toStored TInt
    let store' = insert alloc val' store
    transInstrs insts (funType, (env', store', alloc + 1))
    
    return inter
  IBlock insts -> do
    transInstrs insts inter
    return inter
  IRet exp -> do
    expType <- transExp exp (env, store, alloc)
    if funType == expType then return inter
    else fail $ "Error: RETURN expected " ++ (show funType) ++ ", but got " ++ (show expType)
