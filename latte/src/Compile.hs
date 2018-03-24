module Compile where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import System.IO
import System.IO.Error

import AbsLatte
import PrintLatte
import Static

type StrEnv = Map.Map String String
type Env = Map.Map String EnvElement

-- Konwencja: nazwa w LLVM, potem typy
data EnvElement = Class (Env) -- TODO mem layout, vtable
                | Fn String (Type L) [Type L]
                | Var String (Type L)

compileLatte :: Handle -> Program L -> IO ()
compileLatte file program = do
  emitDeclarations file
  let strings = extractStrings program
  emitStrings file strings
  let env = extractEnv program
  -- TODO class structs and vtable
  -- TODO emit classes
  emitFunctions file env strings program
  emitMain file env

emitDeclarations :: Handle -> IO ()
emitDeclarations file = do
  hPutStrLn file "declare void @printInt(i32)"
  hPutStrLn file "declare void @printString(i8*)"
  hPutStrLn file "declare void @error()"
  hPutStrLn file "declare i32 @readInt()"
  hPutStrLn file "declare i8* @readString()"
  hPutStrLn file "declare i8* @concatString(i8*, i8*)"
  hPutStrLn file "declare i32 @arrlen(i8*)"
  hPutStrLn file "declare i8* @arrmk(i32, i32)"
  hPutStrLn file ""
  hPutStrLn file "declare void @llvm.stackrestore(i8* %ptr)"
  hPutStrLn file "declare i8* @llvm.stacksave()"
  hPutStrLn file ""

extractStrings :: Program L -> StrEnv
extractStrings (Program _ topdef) = fst $ foldl foldTopdef (Map.empty, 0) topdef where
  foldTopdef :: (StrEnv, Int) -> TopDef L -> (StrEnv, Int)
  foldTopdef env (FnDef _ (Func _ _ _ _ (Block _ sts))) = foldl foldStms env sts
  foldTopdef env (ClassDef _ _ _ clsels) = foldl foldCls env clsels
  foldTopdef env _ = env
  
  foldCls :: (StrEnv, Int) -> ClsEl L -> (StrEnv, Int)
  foldCls env (ClassAtt _ _ _) = env
  foldCls env (ClassMet _ (Func _ _ _ _ (Block _ sts))) = foldl foldStms env sts
  
  foldStms :: (StrEnv, Int) -> Stmt L -> (StrEnv, Int)
  foldStms env (BStmt _ (Block _ sts)) = foldl foldStms env sts
  foldStms env (Decl _ _ items) = foldl foldItem env items
  foldStms env (Ass _ _ expr) = foldExpr env expr
  foldStms env (Incr _ expr) = foldExpr env expr
  foldStms env (Decr _ expr) = foldExpr env expr
  foldStms env (Ret _ expr) = foldExpr env expr
  foldStms env (Cond _ expr stmt) = foldStms (foldExpr env expr) stmt
  foldStms env (CondElse _ expr stmt1 stmt2) = foldStms (foldStms (foldExpr env expr) stmt1) stmt2
  foldStms env (While _ expr stmt) = foldStms (foldExpr env expr) stmt
  foldStms env (ForEach _ _ _ expr stmt) = foldStms (foldExpr env expr) stmt
  foldStms env (SExp _ expr) = foldExpr env expr
  foldStms env _ = env
  
  foldItem :: (StrEnv, Int) -> Item L -> (StrEnv, Int)
  foldItem env (NoInit _ _) = env
  foldItem env (Init _ _ expr) = foldExpr env expr
  
  foldExpr :: (StrEnv, Int) -> Expr L -> (StrEnv, Int)
  foldExpr (env, alloc) (EString _ str) = (Map.insert str ("@str_"++(show alloc)) env, alloc + 1)
  foldExpr env (EApp _ e es) = foldl foldExpr env (e:es)
  foldExpr env (EAtt _ expr _) = foldExpr env expr
  foldExpr env (ENewArr _ _ expr) = foldExpr env expr
  foldExpr env (ECast _ _ expr) = foldExpr env expr
  foldExpr env (EArr _ e1 e2) = foldl foldExpr env [e1, e2]
  foldExpr env (Neg _ expr) = foldExpr env expr
  foldExpr env (Not _ expr) = foldExpr env expr
  foldExpr env (EMul _ e1 _ e2) = foldl foldExpr env [e1, e2]
  foldExpr env (EAdd _ e1 _ e2) = foldl foldExpr env [e1, e2]
  foldExpr env (ERel _ e1 _ e2) = foldl foldExpr env [e1, e2]
  foldExpr env (EAnd _ e1 e2) = foldl foldExpr env [e1, e2]
  foldExpr env (EOr _ e1 e2) = foldl foldExpr env [e1, e2]
  foldExpr env _ = env

stringLLVMtype :: String -> String
stringLLVMtype str = let
  str2 = init $ tail str
  str3 = str2++"\\00"
  len = length str2 + 1
  in "["++(show len)++" x i8]"

emitStrings :: Handle -> StrEnv -> IO ()
emitStrings file strings = mapM_ emitString (Map.toAscList strings) >> hPutStrLn file "" where
  emitString (str, name) = let
      str2 = (init $ tail str)++"\\00"
      token = name++" = private unnamed_addr constant "++(stringLLVMtype str)++" c\""++str2++"\""
    in hPutStrLn file token

typeToLLVM :: Type L -> String
typeToLLVM (TypeClass _ (Ident id)) = "i8*"
typeToLLVM (Int _) = "i32"
typeToLLVM (Str _) = "i8*"
typeToLLVM (Bool _) = "i1"
typeToLLVM (Void _) = "void"
typeToLLVM (TypeArray _ t) = (typeToLLVM t)++"*"

extractEnv :: Program L -> Env
extractEnv (Program _ topdef) = functionEnv where -- TODO reconstruct mem layout for classes
  functionEnv = fst $ foldl foldTopfun (defaultEnv, 0) topdef
  
  defaultEnv = Map.fromList defaultFunctions
  defaultFunctions = [
      ("printInt", Fn "@printInt" (Void Nothing) [Int Nothing]),
      ("printString", Fn "@printString" (Void Nothing) [Str Nothing]),
      ("error", Fn "@error" (Void Nothing) []),
      ("readInt", Fn "@readInt" (Int Nothing) []),
      ("readString", Fn "@readString" (Str Nothing) [])
    ]
  
  foldTopfun :: (Env, Int) -> TopDef L -> (Env, Int)
  foldTopfun (env, alloc) (FnDef _ (Func _ ret (Ident id) args _)) =
    (Map.insert id (Fn ("@fun_"++(show alloc)) ret (foldArgs args)) env, alloc + 1)
  foldTopfun env _ = env
  
  foldArgs :: [Arg L] -> [Type L]
  foldArgs args = map argToType args
  
  argToType :: Arg L -> Type L
  argToType (Arg _ t _) = t
  
  -- BIG TODO for classes
  foldCls :: (Env, Int) -> ClsEl L -> (Env, Int)
  foldCls env (ClassAtt _ _ _) = env
  foldCls env (ClassMet _ (Func _ _ _ _ _)) = env

emitFunctions :: Handle -> Env -> StrEnv -> Program L -> IO ()
emitFunctions file env strings (Program _ topdef) = mapM_ emit topdef where
  emit :: TopDef L -> IO ()
  emit (FnDef _ (Func _ t (Ident id) args (Block _ sts))) = do
    let Just (Fn fnreg _ _) = Map.lookup id env
    hPutStr file $ "define "++(typeToLLVM t)++" "++fnreg++"("
    (_, env, alloc) <- foldM emitArgs (True, env, 0) args
    hPutStrLn file ") {"
    hPutStrLn file "entry:"
    (env, alloc) <- foldM emitArgsStore (env, alloc) args
    emitFunctionBody file (env, strings, alloc) sts
    hPutStrLn file "; hax ret"
    case t of
      Void _ -> hPutStrLn file "ret void"
      Str _ -> hPutStrLn file $ "ret "++(typeToLLVM t)++" null"
      TypeArray _ _ -> hPutStrLn file $ "ret "++(typeToLLVM t)++" null"
      TypeClass _ _ -> hPutStrLn file $ "ret "++(typeToLLVM t)++" null"
      _ -> hPutStrLn file $ "ret "++(typeToLLVM t)++" 0"
    hPutStrLn file "}"
    hPutStrLn file ""
  emit _ = return ()
  
  emitArgs :: (Bool, Env, Int) -> Arg L -> IO (Bool, Env, Int)
  emitArgs (isFirst, env2, alloc) (Arg _ t (Ident id)) = do
    if not isFirst then hPutStr file ", " else return ()
    let var_name = "%var_"++(show alloc)
    hPutStr file (typeToLLVM t)
    hPutStr file " "
    hPutStr file var_name
    let env3 = Map.insert id (Var var_name t) env2
    return (False, env3, alloc + 1)
  
  emitArgsStore :: (Env, Int) -> Arg L -> IO (Env, Int)
  emitArgsStore (env, alloc) (Arg _ _ (Ident id)) = do
    let Just (Var name t) = Map.lookup id env
    let reg = "%reg_"++(show alloc)
    let lltype = typeToLLVM t
    hPutStrLn file $ reg++" = alloca "++lltype
    hPutStrLn file $ "store "++lltype++" "++name++", "++lltype++"* "++reg
    let env' = Map.insert id (Var reg t) env
    return (env', alloc + 1)

emitFunctionBody :: Handle -> (Env, StrEnv, Int) -> [Stmt L] -> IO (Env, StrEnv, Int)
emitFunctionBody file env stmts = foldM emitInstruction env stmts where
  emitInstruction :: (Env, StrEnv, Int) -> Stmt L -> IO (Env, StrEnv, Int)
  emitInstruction env (Empty _) = return env
  emitInstruction (env, str, alloc) (BStmt _ (Block _ sts)) = do
    let reg = "%reg_"++(show alloc)
    hPutStrLn file $ reg++" = call i8* @llvm.stacksave()"
    (_, _, alloc) <- emitFunctionBody file (env, str, alloc + 1) sts
    hPutStrLn file $ "call void @llvm.stackrestore(i8* "++reg++")"
    return (env, str, alloc)
  emitInstruction env (Decl _ tp items) = let
      defaultValue (Int _) = "0"
      defaultValue (TypeClass _ _) = "null"
      defaultValue (Str _) = "null"
      defaultValue (TypeArray _ _) = "null"
      defaultValue (Bool _) = "0"
      emitDecl (env, str, alloc) (NoInit _ (Ident id)) = do
        let reg = "%reg_"++(show alloc)
        hPutStrLn file $ reg++" = alloca "++(typeToLLVM tp)
        hPutStrLn file $ "store "++(typeToLLVM tp)++" "++(defaultValue tp)++", "++(typeToLLVM tp)++"* "++reg
        return (Map.insert id (Var reg tp) env, str, alloc + 1)
      emitDecl (env, str, alloc) (Init _ (Ident id) exp) = do
        let reg = "%reg_"++(show alloc)
        hPutStrLn file $ reg++" = alloca "++(typeToLLVM tp)
        (val, _, alloc) <- emitExpr (env, str, alloc + 1) exp
        hPutStrLn file $ "store "++(typeToLLVM tp)++" "++val++", "++(typeToLLVM tp)++"* "++reg
        return (Map.insert id (Var reg tp) env, str, alloc)
    in foldM emitDecl env items
  emitInstruction (env, str, alloc) (Ass _ e1 e2) = do
    (reg, _, alloc) <- emitLvalue (env, str, alloc) e1
    (val, tp, alloc) <- emitExpr (env, str, alloc) e2
    hPutStrLn file $ "store "++(typeToLLVM tp)++" "++val++", "++(typeToLLVM tp)++"* "++reg
    return (env, str, alloc)
  emitInstruction (env, str, alloc) (Incr _ exp) = do
    (reg, _, alloc) <- emitLvalue (env, str, alloc) exp
    let val = "%reg_"++(show alloc)
    hPutStrLn file $ val++" = load i32, i32* "++reg
    let res = "%reg_"++(show (alloc + 1))
    hPutStrLn file $ res++" = add i32 "++val++", 1"
    hPutStrLn file $ "store i32 "++res++", i32* "++reg
    return (env, str, alloc + 2)
  emitInstruction (env, str, alloc) (Decr _ exp) = do
    (reg, _, alloc) <- emitLvalue (env, str, alloc) exp
    let val = "%reg_"++(show alloc)
    hPutStrLn file $ val++" = load i32, i32* "++reg
    let res = "%reg_"++(show (alloc + 1))
    hPutStrLn file $ res++" = sub i32 "++val++", 1"
    hPutStrLn file $ "store i32 "++res++", i32* "++reg
    return (env, str, alloc + 2)
  emitInstruction env@(en, str, _) (Ret _ exp) = do
    (reg, tp, alloc) <- emitExpr env exp
    hPutStrLn file $ "ret "++(typeToLLVM tp)++" "++reg
    return (en, str, alloc)
  emitInstruction env (VRet _) = hPutStrLn file "ret void" >> return env
  emitInstruction (env, strs, alloc) (Cond _ exp stm) = do
    let lblif = "label_"++(show alloc)
    let lblend = "label_"++(show (alloc + 1))
    (reg, _, alloc) <- emitExpr (env, strs, alloc + 2) exp
    hPutStrLn file $ "br i1 "++reg++", label %"++lblif++", label %"++lblend
    hPutStrLn file $ lblif++":"
    (_, _, alloc) <- emitFunctionBody file (env, strs, alloc) [stm]
    hPutStrLn file $ "br label %"++lblend
    hPutStrLn file $ lblend++":"
    return (env, strs, alloc)
  emitInstruction (env, strs, alloc) (CondElse _ exp stm1 stm2) = do
    let lblif = "label_"++(show alloc)
    let lblelse = "label_"++(show (alloc + 1))
    let lblend = "label_"++(show (alloc + 2))
    (reg, _, alloc) <- emitExpr (env, strs, alloc + 3) exp
    hPutStrLn file $ "br i1 "++reg++", label %"++lblif++", label %"++lblelse
    hPutStrLn file $ lblif++":"
    (_, _, alloc) <- emitFunctionBody file (env, strs, alloc) [stm1]
    hPutStrLn file $ "br label %"++lblend
    hPutStrLn file $ lblelse++":"
    (_, _, alloc) <- emitFunctionBody file (env, strs, alloc) [stm2]
    hPutStrLn file $ "br label %"++lblend
    hPutStrLn file $ lblend++":"
    return (env, strs, alloc)
  emitInstruction (env, strs, alloc) (While _ exp stm) = do
    let start = "label_"++(show alloc)
    let mid = "label_"++(show (alloc + 1))
    let end = "label_"++(show (alloc + 2))
    hPutStrLn file $ "br label %"++start
    hPutStrLn file $ start++":"
    (reg, _, alloc) <- emitExpr (env, strs, alloc + 3) exp
    hPutStrLn file $ "br i1 "++reg++", label %"++mid++", label %"++end
    hPutStrLn file $ mid++":"
    (_, _, alloc) <- emitFunctionBody file (env, strs, alloc) [stm]
    hPutStrLn file $ "br label %"++start
    hPutStrLn file $ end++":"
    return (env, strs, alloc)
  emitInstruction (env, str, alloc) (ForEach _ tp (Ident id) exp stm) = do
    let stack = "%reg_"++(show alloc)
    hPutStrLn file $ stack++" = call i8* @llvm.stacksave()"
    (arr, atp, alloc) <- emitExpr (env, str, alloc + 1) exp
    let len = "%reg_"++(show (alloc + 1))
    let var = "%reg_"++(show (alloc + 2))
    let cast = "%reg_"++(show (alloc + 3))
    let it = "%reg_"++(show (alloc + 4))
    hPutStrLn file $ cast++" = bitcast "++(typeToLLVM atp)++" "++arr++" to i8*"
    hPutStrLn file $ len++" = call i32 @arrlen(i8* "++cast++")"
    hPutStrLn file $ it++" = alloca i32"
    hPutStrLn file $ "store i32 0, i32* "++it
    hPutStrLn file $ var++" = alloca "++(typeToLLVM tp)
    
    let condlbl = "label_"++(show (alloc + 5))
    let bodylbl = "label_"++(show (alloc + 6))
    let endlbl = "label_"++(show (alloc + 7))
    hPutStrLn file $ "br label %"++condlbl
    
    hPutStrLn file $ condlbl++":"
    let cond = "%reg_"++(show (alloc + 8))
    let it2 = "%reg_"++(show (alloc + 9))
    hPutStrLn file $ it2++" = load i32, i32* "++it
    hPutStrLn file $ cond++" = icmp slt i32 "++it2++", "++len
    hPutStrLn file $ "br i1 "++cond++", label %"++bodylbl++", label %"++endlbl
    
    hPutStrLn file $ bodylbl++":"
    let arrel = "%reg_"++(show (alloc + 10))
    let t = typeToLLVM tp
    let el = "%reg_"++(show (alloc + 11))
    hPutStrLn file $ arrel++" = getelementptr "++t++", "++t++"* "++arr++", i32 "++it2
    hPutStrLn file $ el++" = load "++t++", "++t++"* "++arrel
    hPutStrLn file $ "store "++t++" "++el++", "++t++"* "++var
    (_, _, alloc) <- emitFunctionBody file (Map.insert id (Var var tp) env, str, alloc + 12) [stm]
    let it3 = "%reg_"++(show alloc)
    hPutStrLn file $ it3++" = add i32 1, "++it2
    hPutStrLn file $ "store i32 "++it3++", i32* "++it
    
    hPutStrLn file $ "br label %"++condlbl
    hPutStrLn file $ endlbl++":"
    
    hPutStrLn file $ "call void @llvm.stackrestore(i8* "++stack++")"
    return (env, str, alloc + 1)
  emitInstruction env@(en, str, _) (SExp _ exp) = do
    (_, _, alloc) <- emitExpr env exp
    return (en, str, alloc)
  
  -- emitExpr -> Rejestr, typ, alloc
  emitExpr :: (Env, StrEnv, Int) -> Expr L -> IO (String, Type L, Int)
  emitExpr (_, _, alloc) (ELitInt _ n) = return (show n, Int Nothing, alloc)
  emitExpr (_, _, alloc) (ELitTrue _) = return ("1", Bool Nothing, alloc)
  emitExpr (_, _, alloc) (ELitFalse _) = return ("0", Bool Nothing, alloc)
  emitExpr (_, strs, alloc) (EString _ str) = do
    let Just var = Map.lookup str strs
    let reg = "%reg_"++(show alloc)
    let lltype = stringLLVMtype str
    hPutStrLn file $ reg++" = getelementptr "++lltype++", "++lltype++"* "++var++", i32 0, i64 0"
    return (reg, Str Nothing, alloc + 1)
  emitExpr (env, _, alloc) (EVar _ (Ident id)) = do
    let Just el = Map.lookup id env
    case el of
      Fn reg ret args -> return (reg, Fun Nothing ret args, alloc)
      Var reg t -> do
        let reg2 = "%reg_"++(show alloc)
        hPutStrLn file $ reg2++" = load "++(typeToLLVM t)++", "++(typeToLLVM t)++"* "++reg
        return (reg2, t, alloc + 1)
      -- Class xxx -- TODO
      -- Help https://gist.github.com/robstewart57/b11353feb69dc1a6dc30#file-c-to-llvm-L50
  emitExpr en@(env, st, _) (EApp _ fun args) = do
    (funreg, Fun _ fres fargs, alloc) <- emitExpr en fun
    let folder (al, r) a = emitExpr (env, st, al) a >>= (\(reg, tp, all) -> return (all, (reg, tp):r))
    (alloc, args) <- foldM folder (alloc, []) args
    let args' = reverse args
    -- TODO class casting via bitcast on (zipWith args' fargs) ???
    let reg = "%reg_"++(show alloc)
    case fres of
      Void _ -> hPutStr file $ "call "++(typeToLLVM fres)++" "++funreg++"("
      _ -> hPutStr file $ reg++" = call "++(typeToLLVM fres)++" "++funreg++"("
    foldM emitFunArgs False args'
    hPutStrLn file ")"
    return (reg, fres, alloc + 1)
  emitExpr env (Neg _ exp) = do
    (reg, tp, alloc) <- emitExpr env exp
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = sub "++(typeToLLVM tp)++" 0, "++reg
    return (res, tp, alloc + 1)
  emitExpr env (Not _ exp) = do
    (reg, tp, alloc) <- emitExpr env exp
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = sub "++(typeToLLVM tp)++" 1, "++reg
    return (res, tp, alloc + 1)
  emitExpr (env, str, alloc) (EMul _ e1 op e2) = do
    op <- case op of
      Times _ -> return "mul"
      Div _ -> return "sdiv"
      Mod _ -> return "srem"
    (reg1, tp, alloc) <- emitExpr (env, str, alloc) e1
    (reg2, tp, alloc) <- emitExpr (env, str, alloc) e2
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = "++op++" "++(typeToLLVM tp)++" "++reg1++", "++reg2
    return (res, tp, alloc + 1)
  emitExpr (env, str, alloc) (EAdd _ e1 op e2) = do
    (reg1, tp, alloc) <- emitExpr (env, str, alloc) e1
    (reg2, tp, alloc) <- emitExpr (env, str, alloc) e2
    let res = "%reg_"++(show alloc)
    op <- case (tp, op) of
      (Int _, Plus _)  -> hPutStrLn file $ res++" = add "++(typeToLLVM tp)++" "++reg1++", "++reg2
      (Int _, Minus _) -> hPutStrLn file $ res++" = sub "++(typeToLLVM tp)++" "++reg1++", "++reg2
      (Str _, Plus _)  -> hPutStrLn file $ res++" = call i8* @concatString(i8* "++reg1++", i8* "++reg2++")"
      _ -> fail "Code generator error"
    return (res, tp, alloc + 1)
  emitExpr (env, str, alloc) (ERel _ e1 op e2) = do
    op <- case op of
      LTH _ -> return "slt"
      LE _ -> return "sle"
      GTH _ -> return "sgt"
      GE _ -> return "sge"
      EQU _ -> return "eq"
      NE _ -> return "ne"
    (reg1, tp, alloc) <- emitExpr (env, str, alloc) e1
    (reg2, tp, alloc) <- emitExpr (env, str, alloc) e2
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = icmp "++op++" "++(typeToLLVM tp)++" "++reg1++", "++reg2
    return (res, Bool Nothing, alloc + 1)
  emitExpr (env, str, alloc) (EAnd _ e1 e2) = do
    let start = "label_"++(show alloc)
    (reg1, _, alloc) <- emitExpr (env, str, alloc + 1) e1
    hPutStrLn file $ "br label %"++start
    hPutStrLn file $ start++":"
    let mid = "label_"++(show alloc)
    let midend = "label_"++(show (alloc + 1))
    let end = "label_"++(show (alloc + 2))
    hPutStrLn file $ "br i1 "++reg1++", label %"++mid++", label %"++end
    hPutStrLn file $ mid++":"
    (reg2, _, alloc) <- emitExpr (env, str, alloc + 3) e2
    hPutStrLn file $ "br label %"++midend
    hPutStrLn file $ midend++":"
    hPutStrLn file $ "br label %"++end
    hPutStrLn file $ end++":"
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = phi i1 [ 0, %"++start++" ], [ "++reg2++", %"++midend++" ]"
    return (res, Bool Nothing, alloc + 1)
  emitExpr (env, str, alloc) (EOr _ e1 e2) = do
    let start = "label_"++(show alloc)
    (reg1, _, alloc) <- emitExpr (env, str, alloc + 1) e1
    hPutStrLn file $ "br label %"++start
    hPutStrLn file $ start++":"
    let mid = "label_"++(show alloc)
    let midend = "label_"++(show (alloc + 1))
    let end = "label_"++(show (alloc + 2))
    hPutStrLn file $ "br i1 "++reg1++", label %"++end++", label %"++mid
    hPutStrLn file $ mid++":"
    (reg2, _, alloc) <- emitExpr (env, str, alloc + 3) e2
    hPutStrLn file $ "br label %"++midend
    hPutStrLn file $ midend++":"
    hPutStrLn file $ "br label %"++end
    hPutStrLn file $ end++":"
    let res = "%reg_"++(show alloc)
    hPutStrLn file $ res++" = phi i1 [ 1, %"++start++" ], [ "++reg2++", %"++midend++" ]"
    return (res, Bool Nothing, alloc + 1)
  emitExpr (env, str, alloc) (ENewArr _ tp exp) = do
    (size, _, alloc) <- emitExpr (env, str, alloc) exp
    let elsize = show $ getMemSize env tp
    let arr = "%reg_"++(show alloc)
    let cast = "%reg_"++(show (alloc + 1))
    hPutStrLn file $ arr++" = call i8* @arrmk(i32 "++size++", i32 "++elsize++")"
    hPutStrLn file $ cast++" = bitcast i8* "++arr++" to "++(typeToLLVM tp)++"*"
    return (cast, TypeArray Nothing tp, alloc + 2)
  emitExpr (env, str, alloc) (EArr _ arr ind) = do
    (arr, TypeArray _ tp, alloc) <- emitExpr (env, str, alloc) arr
    (ind, _, alloc) <- emitExpr (env, str, alloc) ind
    let ptr = "%reg_"++(show alloc)
    let res = "%reg_"++(show (alloc + 1))
    let t = typeToLLVM tp
    hPutStrLn file $ ptr++" = getelementptr "++t++", "++t++"* "++arr++", i32 "++ind
    hPutStrLn file $ res++" = load "++t++", "++t++"* "++ptr
    return (res, tp, alloc + 2)
  emitExpr (env, str, alloc) (EAtt _ exp (Ident id)) = do
    (reg, tp, alloc) <- emitExpr (env, str, alloc) exp
    case (tp, id) of
      -- TODO fields of class
      (TypeArray _ _, "length") -> do
        let cast = "%reg_"++(show alloc)
        let len = "%reg_"++(show (alloc + 1))
        hPutStrLn file $ cast++" = bitcast "++(typeToLLVM tp)++" "++reg++" to i8*"
        hPutStrLn file $ len++" = call i32 @arrlen(i8* "++cast++")"
        return (len, Int Nothing, alloc + 2)
  {-| ENew a (Type a)
    | ECast a (Type a) (Expr a)
    | ENull a 
    -}
  emitFunArgs isNotFirst (reg, tp) = do
    if isNotFirst then hPutStr file ", " else return ()
    hPutStr file (typeToLLVM tp)
    hPutStr file " "
    hPutStr file reg
    return True
  
  emitLvalue :: (Env, StrEnv, Int) -> Expr L -> IO (String, Type L, Int)
  emitLvalue (env, _, alloc) (EVar _ (Ident id)) = do
    let Just (Var reg t) = Map.lookup id env
    return (reg, t, alloc)
  emitLvalue (env, str, alloc) (EArr _ arr ind) = do
    (arr, TypeArray _ tp, alloc) <- emitExpr (env, str, alloc) arr
    (ind, _, alloc) <- emitExpr (env, str, alloc) ind
    let ptr = "%reg_"++(show alloc)
    let t = typeToLLVM tp
    hPutStrLn file $ ptr++" = getelementptr "++t++", "++t++"* "++arr++", i32 "++ind
    return (ptr, tp, alloc + 1)
  --emitLvalue (env, str, alloc) (EAtt _ expr field) = -- TODO

getMemSize :: Env -> Type L -> Int
--getMemSize env (TypeClass _ (Ident id)) = 0 -- TODO!!!
getMemSize _ (Int _) = 4
getMemSize _ (Str _) = 8
getMemSize _ (Bool _) = 1
getMemSize _ (TypeArray _ _) = 8

emitMain :: Handle -> Env -> IO ()
emitMain file env = do
  let Just (Fn fnreg _ _) = Map.lookup "main" env
  hPutStrLn file "define i32 @main() {"
  hPutStrLn file $ "  %r = call i32 "++fnreg++"()"
  hPutStrLn file "  ret i32 %r"
  hPutStrLn file "}"
