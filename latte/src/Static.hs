module Static where

import Control.Monad (foldM)
import Data.List ( (\\), find )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO
import System.IO.Error

import AbsLatte
import PrintLatte

type L = Maybe (Int, Int)

doStaticCheck :: Program L -> IO ()
doStaticCheck (Program _ defs) = do
  env <- collectBasicEnv defs
  checkClassHierarchy env
  checkMainFunction env
  checkFunctionsHeaders env
  checkFunctions env

showPos :: L -> String
showPos Nothing = "unknown position in code"
showPos (Just (line, ch)) = "line " ++ (show line) ++ ", column " ++ (show ch)

prnt :: Print p => p -> String
prnt v = render $ prt 0 v

collectBasicEnv :: [TopDef L] -> IO (Map.Map String (TopDef L))
collectBasicEnv defs = go defs defaultEnv where
  -- Default functions defined in language
  defaultEnv = Map.fromList [
      mkFun "printInt" [Int] Void,
      mkFun "printString" [Str] Void,
      mkFun "error" [] Void,
      mkFun "readInt" [] Int,
      mkFun "readString" [] Str
    ]
  mkFun name args ret = (name, FnDef Nothing (Func Nothing (ret Nothing) (Ident name) (mkArgs args) (Block Nothing [])))
  mkArgs args = map mkArg args
  mkArg t = Arg Nothing (t Nothing) (Ident "arg")
  go [] env = return env
  go (d:ds) env = do
    (name, pos) <- case d of
      FnDef p (Func _ _ (Ident id) _ _) -> return (id, p)
      ClassDef p (Ident id) _ _ -> return (id, p)
    if Map.member name env then
      fail $ "Redefinition of function/class \""++name++"\" in " ++ (showPos pos)
      else return ()
    go ds (Map.insert name d env)

isClass :: TopDef L -> Bool
isClass (FnDef _ _) = False
isClass (ClassDef _ _ _ _) = True

checkPredecessor :: Map.Map String (TopDef L) -> TopDef L -> IO ()
checkPredecessor env (ClassDef _ _ (ClassExt pos (Ident par)) _) =
  case Map.lookup par env of
    Just _ -> return ()
    Nothing -> fail $ "Class \"" ++ par ++ "\" doesn't exist. In " ++ (showPos pos)
checkPredecessor env _ = return ()

classTopoSort :: Map.Map String (TopDef L) -> [String]
classTopoSort cls = fst $ Map.foldl sorter ([], Map.empty) cls where
  sorter :: ([String], Map.Map String ()) -> TopDef L -> ([String], Map.Map String ())
  sorter (l, vis) (ClassDef _ (Ident id) (ClassExt _ (Ident par)) _) =
    if Map.member id vis then (l, vis)
    else let Just parent = Map.lookup par cls
    in let (l2, vis2) = sorter (l, Map.insert id () vis) parent
    in (id:l2, vis2)
  sorter (l, vis) (ClassDef _ (Ident id) (ClassNoEx _) _) =
    if Map.member id vis then (l, vis)
    else (id:l, Map.insert id () vis)

detectClassCycle :: Map.Map String (TopDef L) -> [String] -> Maybe String
detectClassCycle env sorted = go sorted Map.empty where
  go (s:ss) vis =
    let vis2 = Map.insert s () vis in
    let Just (ClassDef _ _ ext _) = Map.lookup s env in
    case ext of
      ClassNoEx _ -> go ss vis2
      ClassExt _ (Ident par) ->
        if Map.member par vis2 then Just $ getClassCycle par env
        else go ss vis2
  go [] _ = Nothing

getClassCycle :: String -> Map.Map String (TopDef L) -> String
getClassCycle start env = start ++ " -> " ++ (go (getParent start)) ++ start where
  getParent id = let Just (ClassDef _ _ (ClassExt _ (Ident par)) _) = Map.lookup id env in par
  go id =
    if id == start then "" 
    else id ++ " -> " ++ (go $ getParent id)

checkClassInheritedFields :: Map.Map String (TopDef L) -> [String] -> IO ()
checkClassInheritedFields env sorted = go sorted Map.empty where
  go :: [String] -> Map.Map String (Map.Map String (ClsEl L)) -> IO ()
  go [] _ = return ()
  go (s:ss) layout = do
    let Just (ClassDef _ _ ext els) = Map.lookup s env
    let fields = case ext of {
      ClassExt _ (Ident par) -> let Just res = Map.lookup par layout in res ;
      ClassNoEx _ -> Map.empty }
    fields <- fillFields els fields s
    go ss (Map.insert s fields layout)

  fillFields :: [ClsEl L] -> Map.Map String (ClsEl L) -> String -> IO (Map.Map String (ClsEl L))
  fillFields [] fields _ = return fields
  fillFields (e:es) fields cls = do
    let name = classFieldName e
    case Map.lookup name fields of
      Nothing -> return ()
      Just (ClassAtt f _ _) -> fail $ err2 (classFieldPos e) f (name++" of class "++cls)
      Just f -> compareClassFields e f (name++" of class "++cls)
    fillFields es (Map.insert name e fields) cls

  err1 new old name =
    "Field "++name++" redefined in "++(showPos new)++" doesn't match original definition in "++(showPos old)
  err2 new old name =
    "Field "++name++" in "++(showPos new)++" redefines original attribute in "++(showPos old)

  compareClassFields :: ClsEl L -> ClsEl L -> String -> IO ()
  compareClassFields (ClassMet p1 fun1) (ClassMet p2 fun2) name =
    if functionsEqual fun1 fun2 then return () else fail $ err1 p1 p2 name
  compareClassFields (ClassAtt p1 t1 _) (ClassAtt p2 t2 _) name =
    if typesEqualStrict t1 t2 then return () else fail $ err1 p1 p2 name
  compareClassFields new old name = fail $ err1 (classFieldPos new) (classFieldPos old) name

functionsEqual (Func _ t1 _ a1 _) (Func _ t2 _ a2 _) =
  typesEqualStrict t1 t2 && argsEqual a1 a2

argsEqual ((Arg _ t1 _):a1) ((Arg _ t2 _):a2) =
  typesEqualStrict t1 t2 && argsEqual a1 a2
argsEqual [] [] = True
argsEqual _ _ = False

typesEqualStrict (TypeClass _ t1) (TypeClass _ t2) = t1 == t2
typesEqualStrict (Int _) (Int _) = True
typesEqualStrict (Str _) (Str _) = True
typesEqualStrict (Bool _) (Bool _) = True
typesEqualStrict (Void _) (Void _) = True
typesEqualStrict (TypeArray _ t1) (TypeArray _ t2) = typesEqualStrict t1 t2
typesEqualStrict (Fun _ t1 a1) (Fun _ t2 a2) =
  typesEqualStrict t1 t2 && all (\x -> x) (zipWith typesEqualStrict a1 a2)
typesEqualStrict _ _ = False

typesEqual env (TypeClass _ t1) (TypeClass _ t2@(Ident id)) = t1 == t2 || case Map.lookup id env of
  Just (ClassDef _ _ (ClassExt _ id) _) -> typesEqual env (TypeClass Nothing t1) (TypeClass Nothing id)
  _ -> False
typesEqual _ (Int _) (Int _) = True
typesEqual _ (Str _) (Str _) = True
typesEqual _ (Bool _) (Bool _) = True
typesEqual _ (Void _) (Void _) = True
typesEqual env (TypeArray _ t1) (TypeArray _ t2) = typesEqual env t1 t2
typesEqual env (Fun _ t1 a1) (Fun _ t2 a2) =
  typesEqual env t1 t2 && length a1 == length a2 && all (\x -> x) (zipWith (typesEqual env) a1 a2)
typesEqual _ _ _ = False

classFieldName :: ClsEl L -> String
classFieldName (ClassAtt _ _ (Ident id)) = id
classFieldName (ClassMet _ (Func _ _ (Ident id) _ _)) = id

classFieldPos :: ClsEl L -> L
classFieldPos (ClassAtt p _ _) = p
classFieldPos (ClassMet p _) = p

checkClassHierarchy :: Map.Map String (TopDef L) -> IO ()
checkClassHierarchy env = do
  let classes = Map.filter isClass env
  -- Is inheritance correct
  mapM_ (checkPredecessor classes) classes
  -- Look for cycles in inheritance graph
  let sorted = classTopoSort classes
  case detectClassCycle env sorted of
    Nothing -> return ()
    Just cycle -> fail $ "Class inheritance cycle detected: " ++ cycle
  -- Check inherited methods and fields
  checkClassInheritedFields env (reverse sorted)

checkMainFunction :: Map.Map String (TopDef L) -> IO ()
checkMainFunction env = do
  fun <- case Map.lookup "main" env of
    Nothing -> fail "int main() function not found!"
    Just (ClassDef p _ _ _) -> fail $ "\"main\" must be a function, not class! In "++(showPos p)
    Just (FnDef _ fun) -> return fun
  case fun of
    Func _ (Int _) _ [] _ -> return ()
    Func p _ _ (_:_) _ -> fail $ "\"main\" must not take any arguments! In "++(showPos p)
    Func p _ _ _ _ -> fail $ "\"main\" have to return int! In "++(showPos p)

repeated :: Ord a => [a] -> [a]
repeated l = let dist = Set.toList $ Set.fromList l in l \\ dist

checkFunctionsHeaders :: Map.Map String (TopDef L) -> IO ()
checkFunctionsHeaders env = mapM_ checkObject env where
  checkTp :: Type L -> Bool
  checkTp t = checkType env t

  checkObject :: TopDef L -> IO ()
  checkObject (FnDef _ fun) = checkFunction fun
  checkObject (ClassDef p (Ident name) _ elems) = do
    mapM_ checkElem elems
    let bad = repeated $ map classFieldName elems
    case bad of
      [] -> return ()
      (s:_) -> fail $ "Repeated function body \""++s++"\" in class "++name++", "++ (showPos p)

  checkFunction (Func p t1 (Ident f) args _) = do
    if checkTp t1 || isVoid t1 then return () else fail $ "Invalid return type in function "++f++", "++ (showPos p)
    mapM_ checkArg args
    let bad = repeated $ map (\(Arg _ _ (Ident n)) -> n) args
    case bad of
      [] -> return ()
      (s:_) -> fail $ "Repeated argument \""++s++"\" in function "++f++", "++ (showPos p)

  checkElem (ClassMet _ fun) = checkFunction fun
  checkElem (ClassAtt p t (Ident n)) =
    if checkTp t then return () else fail $ "Invalid type of class attribute "++n++" in "++ (showPos p)

  checkArg (Arg p t (Ident n)) =
    if checkTp t then return () else fail $ "Invalid type of function argument "++n++" in "++ (showPos p)

isVoid (Void _) = True
isVoid _ = False

checkType env (TypeClass _ (Ident i)) = case Map.lookup i env of
  Just (ClassDef _ _ _ _) -> True
  _ -> False
checkType env (TypeArray _ t) = checkType env t
checkType _ (Void _) = False
checkType _ _ = True

checkFunctions :: Map.Map String (TopDef L) -> IO ()
checkFunctions env = mapM_ (checkBlocks env) env

checkBlocks :: Map.Map String (TopDef L) -> TopDef L -> IO ()
checkBlocks env (FnDef _ fun@(Func _ _ (Ident n) _ _)) =
  if elem n ["readInt", "readString", "error", "printInt", "printString"] then return ()
  else checkFunBlock env Nothing fun
checkBlocks env (ClassDef _ (Ident inside) _ es) = mapM_ (checkElBlock env inside) es

checkElBlock _ _ (ClassAtt _ _ _) = return ()
checkElBlock env inside (ClassMet _ fun) = checkFunBlock env (Just inside) fun

checkFunBlock :: Map.Map String (TopDef L) -> Maybe String -> Function L -> IO ()
checkFunBlock ext inside (Func p ret (Ident n) args (Block _ stmts)) = do
  let fenv = Map.fromList $ foldl (\l (Arg _ t (Ident i)) -> (i, t):l) [] args
  case ret of
    Void _ -> return ()
    _ -> checkReturns (n, p) stmts
  ext <- case inside of
    Just cls -> let Just ins = Map.lookup cls ext in return $ Map.insert "self" ins ext
    Nothing -> return ext
  foldM (checkFunStmt inside ext ret) fenv stmts
  return ()

checkFunStmt _ _ _ fenv (Empty _) = return fenv
checkFunStmt inside ext ret fenv (BStmt _ (Block _ sts)) = do
  let f k v res = Map.insert k (SpecVar Nothing v) res
  let ext2 = Map.foldrWithKey f ext fenv
  foldM (checkFunStmt inside ext2 ret) Map.empty sts
  return fenv
checkFunStmt inside ext ret fenv (Decl p t items) = do
  let checkClassType id = case envIdLookup inside ext id of {
    Just (TypeClass _ _) -> return ();
    _ -> fail $ id ++" is not a class! In "++(showPos p)
  }
  case t of
    TypeClass _ (Ident id) -> checkClassType id
    TypeArray _ (TypeClass _ (Ident id)) -> checkClassType id
    _ -> return ()
  let isItemCorrect it = case it of {
    NoInit _ _ -> return () ;
    Init p2 (Ident idd) expr -> do
      itt <- getExpressionType inside ext fenv expr
      if typesEqual ext t itt then return () else
        fail $ "Incorrect type of item in definition of "++idd++". In "++(showPos p2)
    }
  mapM_ isItemCorrect items
  let insertItem fe i = case i of {
    NoInit pp (Ident id) ->
      if Map.member id fe then fail $ "Variable already declared: "++id++". In "++(showPos pp)
      else return $ Map.insert id t fe ;
    Init pp (Ident id) _ ->
      if Map.member id fe then fail $ "Variable already declared: "++id++". In "++(showPos pp)
      else return $ Map.insert id t fe
    }
  foldM insertItem fenv items
checkFunStmt inside ext ret fenv (Ass p el er) = do
  isLvalue el
  t1 <- getExpressionType inside ext fenv el
  t2 <- getExpressionType inside ext fenv er
  if typesEqual ext t1 t2 then return () else fail $ "Type mismatch in assignment! In "++(showPos p)
  return fenv
checkFunStmt inside ext ret fenv (Incr p expr) = do
  isLvalue expr
  t <- getExpressionType inside ext fenv expr
  case t of
    Int _ -> return ()
    _ -> fail $ "Operator ++ supports only Ints. In " ++ (showPos p)
  return fenv
checkFunStmt inside ext ret fenv (Decr p expr) = do
  isLvalue expr
  t <- getExpressionType inside ext fenv expr
  case t of
    Int _ -> return ()
    _ -> fail $ "Operator -- supports only Ints. In " ++ (showPos p)
  return fenv
checkFunStmt inside ext ret fenv (Ret p expr) = do
  t <- getExpressionType inside ext fenv expr
  if typesEqual ext t ret then return fenv
    else fail $ "Incorrect type in return statement in"++(showPos p)++"\nExpected "++(prnt ret)++", got "++(prnt t)
checkFunStmt _ ext ret fenv (VRet p) =
  if typesEqual ext ret (Void Nothing) then return fenv
  else fail $ "Return expects argument in "++(showPos p)
checkFunStmt inside ext ret fenv (Cond p cond sts) = do
  t <- getExpressionType inside ext fenv cond
  case t of
    Bool _ -> return ()
    _ -> fail $ "Condition must be bool! In "++(showPos p)
  checkFunStmt inside ext ret fenv sts
  return fenv
checkFunStmt inside ext ret fenv (CondElse p cond s1 s2) = do
  t <- getExpressionType inside ext fenv cond
  case t of
    Bool _ -> return ()
    _ -> fail $ "Condition must be bool! In "++(showPos p)
  checkFunStmt inside ext ret fenv s1
  checkFunStmt inside ext ret fenv s2
  return fenv
checkFunStmt inside ext ret fenv (While p cond sts) = do
  t <- getExpressionType inside ext fenv cond
  case t of
    Bool _ -> return ()
    _ -> fail $ "Condition must be bool! In "++(showPos p)
  checkFunStmt inside ext ret fenv sts
  return fenv
checkFunStmt inside ext ret fenv (ForEach p tv (Ident id) expr sts) = do
  te <- getExpressionType inside ext fenv expr
  te2 <- case te of
    TypeArray _ t -> return t
    _ -> fail $ "Foreach loop allows iterating only over array! In "++(showPos p)
  if typesEqual ext te2 tv then return () else fail $ "Types mismatch in foreach loop! In "++(showPos p)
  checkFunStmt inside ext ret (Map.insert id tv fenv) sts
  return fenv
checkFunStmt inside ext ret fenv (SExp _ expr) = do
  getExpressionType inside ext fenv expr
  return fenv

nullType = "%@!NULL" :: String

getExpressionType :: Maybe String -> Map.Map String (TopDef L) -> Map.Map String (Type L) -> Expr L -> IO (Type L)
getExpressionType _ _ _ (ELitInt _ _) = return $ Int Nothing
getExpressionType _ _ _ (ELitTrue _) = return $ Bool Nothing
getExpressionType _ _ _ (ELitFalse _) = return $ Bool Nothing
getExpressionType _ _ _ (EString _ _) = return $ Str Nothing
getExpressionType _ _ _ (ENull _) = return $ TypeClass Nothing (Ident nullType)
getExpressionType inside ext env (EVar p (Ident id)) =
  case Map.lookup id env of
    Just t -> return t
    Nothing -> case envIdLookup inside ext id of
      Just t -> return t
      Nothing -> fail $ "Unknown variable "++id++". In "++(showPos p)
getExpressionType inside ext env (Neg p expr) = do
  t <- getExpressionType inside ext env expr
  case t of
    Int _ -> return $ Int Nothing
    _ -> fail $ "Negation expects Int value. In "++(showPos p)
getExpressionType inside ext env (Not p expr) = do
  t <- getExpressionType inside ext env expr
  case t of
    Bool _ -> return $ Bool Nothing
    _ -> fail $ "\"NOT\" expects value of type Bool. In "++(showPos p)
getExpressionType inside ext env (EMul p e1 _ e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case t1 of
    Int _ -> return ()
    _ -> fail $ "Multiplication expects value of type Int. In "++(showPos p)
  case t2 of
    Int _ -> return $ Int Nothing
    _ -> fail $ "Multiplication expects value of type Int. In "++(showPos p)
getExpressionType inside ext env (ERel p e1 _ e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case (t1, t2) of
    (Int _, Int _) -> return $ Bool Nothing
    (Bool _, Bool _) -> return $ Bool Nothing
    (TypeClass _ (Ident i1), TypeClass _ (Ident i2)) ->
      if i1 == i2 then return $ Bool Nothing else fail $ "Classes not comparable. In "++(showPos p)
    _ -> fail $ "Types not comparable. In "++(showPos p)
getExpressionType inside ext env (EAdd p e1 (Plus _) e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case (t1, t2) of
    (Int _, Int _) -> return $ Int Nothing
    (Str _, Str _) -> return $ Str Nothing
    _ -> fail $ "Add expects both values to be either Ints or Strings. In "++(showPos p)
getExpressionType inside ext env (EAdd p e1 _ e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case (t1, t2) of
    (Int _, Int _) -> return $ Int Nothing
    _ -> fail $ "Add expects both values to be Ints. In "++(showPos p)
getExpressionType inside ext env (EAnd p e1 e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case (t1, t2) of
    (Bool _, Bool _) -> return $ Bool Nothing
    _ -> fail $ "Add expects both values to be Bools. In "++(showPos p)
getExpressionType inside ext env (EOr p e1 e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case (t1, t2) of
    (Bool _, Bool _) -> return $ Bool Nothing
    _ -> fail $ "Add expects both values to be Bools. In "++(showPos p)
getExpressionType _ ext _ (ENew p t) =
  case t of
    TypeClass _ (Ident id) -> case Map.lookup id ext of
      Just (ClassDef _ _ _ _) -> return t
      _ -> fail $ "Unknown class "++id++". In "++(showPos p)
    _ -> fail $ "New expects name of class or array type. In "++(showPos p)
getExpressionType inside ext env (EArr p e1 e2) = do
  t1 <- getExpressionType inside ext env e1
  t2 <- getExpressionType inside ext env e2
  case t2 of
    Int _ -> return ()
    _ -> fail $ "Arrays must be indexed with integers. In "++(showPos p)
  case t1 of
    TypeArray _ t3 -> return t3
    _ -> fail $ "Array expected. In "++(showPos p)
getExpressionType inside ext env (ENewArr p t e) = do
  te <- getExpressionType inside ext env e
  case te of
    Int _ -> return ()
    _ -> fail $ "Array size must be integer. In "++(showPos p)
  return $ TypeArray Nothing t
getExpressionType inside ext env (EApp p expr args) = do
  fun <- getExpressionType inside ext env expr
  (r, argt) <- case fun of
    Fun _ t at -> return (t, at)
    _ -> fail $ "Call is available only on functions! In "++(showPos p)
  args <- mapM (getExpressionType inside ext env) args
  if length argt == length args then return ()
    else fail $ "This call expects "++(show $ length argt)++" arguments! In "++(showPos p)
  if all (\x -> x) $ zipWith (typesEqual ext) args argt then return ()
    else fail $ "Incorrect types of arguments in function invocation! In "++(showPos p)
  return r
getExpressionType inside ext env (ECast p cst expr) = do
  t <- getExpressionType inside ext env expr
  case t of
    TypeClass _ _ -> return ()
    _ -> fail $ "Casting allowed only for classes! In "++(showPos p)
  let TypeClass _ (Ident className) = t
  if typesEqual ext cst t || className == nullType then return cst
    else fail $ "Can't perform casting. In "++(showPos p)
getExpressionType inside ext env (EAtt p expr (Ident att)) = do
  t <- getExpressionType inside ext env expr
  case t of
    TypeArray _ _ -> if att == "length" then return $ Int Nothing
      else fail $ "Arrays have \"length\" attribute only! In "++(showPos p)
    TypeClass _ (Ident cls) -> case envIdLookup (Just cls) ext att of
      Nothing -> fail $ "Unknown attribute "++att++"! In "++(showPos p)
      Just t -> return t
    _ -> fail $ "Only classes and arrays have attributes! In "++(showPos p)

isLvalue :: Expr L -> IO ()
isLvalue (ELitInt p _) = fail $ "Constants not allowed in Lvalues! In "++(showPos p)
isLvalue (ELitTrue p) = fail $ "Constants not allowed in Lvalues! In "++(showPos p)
isLvalue (ELitFalse p) = fail $ "Constants not allowed in Lvalues! In "++(showPos p)
isLvalue (EString p _) = fail $ "Constants not allowed in Lvalues! In "++(showPos p)
isLvalue (ENull p) = fail $ "Constants not allowed in Lvalues! In "++(showPos p)
isLvalue (EVar p (Ident id)) = return ()
isLvalue (Neg p _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (Not p _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EMul p _ _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (ERel p _ _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EAdd p _ _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EAnd p _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EOr p _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (ENew p _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EArr p e _) = isLvalue e
isLvalue (ENewArr p _ _) = fail $ "Expressions not allowed in Lvalues! In "++(showPos p)
isLvalue (EApp p _ _) = fail $ "Calls not allowed in Lvalues! In "++(showPos p)
isLvalue (EAtt _ expr _) = isLvalue expr
isLvalue (ECast p _ _) = fail $ "Casting not allowed in Lvalues! In "++(showPos p)

envIdLookup :: Maybe String -> Map.Map String (TopDef L) -> String -> Maybe (Type L)
envIdLookup Nothing e v = case Map.lookup v e of
  Nothing -> Nothing
  Just (ClassDef _ id _ _) -> Just $ TypeClass Nothing id
  Just (FnDef _ (Func _ t _ args _)) -> Just $ Fun Nothing t (map (\(Arg _ tp _) -> tp) args)
  Just (SpecVar _ t) -> Just t
envIdLookup (Just inside) ext id =
  let Just (ClassDef _ _ clsext els) = Map.lookup inside ext
  in let fin e = case e of {
    ClassMet _ (Func _ _ (Ident i) _ _) -> id == i ;
    ClassAtt _ _ (Ident i) -> id == i }
  in let el = find fin els
  in case (el, clsext) of
    (Just (ClassAtt _ t _), _) -> Just t
    (Just (ClassMet _ (Func _ t _ args _)), _) -> Just $ Fun Nothing t (map (\(Arg _ tp _) -> tp) args)
    (_, ClassNoEx _) -> envIdLookup Nothing ext id
    (_, ClassExt _ (Ident sup)) -> envIdLookup (Just sup) ext id

checkReturns (n, p) [] = fail $ "Function "++n++" defined in "++(showPos p)++" have missing return statements"
checkReturns (n, p) (s:ss) = do
  let catch err = if null ss then ioError err else checkReturns (n, p) ss
  let isTrue e = case e of {
    ELitTrue _ -> True ;
    Not _ e1 -> not $ isTrue e1 ;
    EAnd _ e1 e2 ->  isTrue e1 && isTrue e2;
    EOr _ e1 e2 ->  isTrue e1 || isTrue e2;
    _ -> False
  }
  let isFalse e = case e of {
    ELitFalse _ -> True ;
    Not _ e1 -> not $ isFalse e1 ;
    EAnd _ e1 e2 ->  isFalse e1 || isFalse e2;
    EOr _ e1 e2 ->  isFalse e1 && isFalse e2;
    _ -> False
  }
  case s of
    BStmt _ (Block a b) -> checkReturns (n, a) b `catchIOError` catch
    Ret a _ -> if null ss then return () else fail $ "Unreachable statements after return in "++n++", "++(showPos a)
    VRet r -> fail $ "Empty return statement in "++(showPos r)
    Cond _ cnd stmt ->
      if isTrue cnd then checkReturns (n, p) [stmt] `catchIOError` catch else checkReturns (n, p) ss
    CondElse _ cnd s1 s2 -> do
      if isFalse cnd then return () else checkReturns (n, p) [s1] `catchIOError` catch
      if isTrue cnd then return () else checkReturns (n, p) [s2] `catchIOError` catch
    _ -> checkReturns (n, p) ss

