module Main where

import qualified Data.Map.Strict as Map
import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit
import System.FilePath
import System.Process

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant

import ErrM

type ParseFun a = [Token] -> Err a

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run f

run :: FilePath -> String -> IO ()
run f s = let ts = myLexer s in case pProgram ts of
           Bad s   -> do putStrLn "Parse Failed..."
                         putStrLn "Tokens:"
                         putStrLn $ show ts
                         putStrLn s
                         exitWith (ExitFailure 1)
           Ok tree -> do
             let out = (dropExtension f) ++ ".ll"
             file <- openFile out WriteMode
             generateCode file (takeBaseName f) tree
             hFlush file
             hClose file
             cmd <- runCommand $ "llvm-as " ++ out
             _ <- waitForProcess cmd
             return ()

generateCode :: Handle -> String -> Program -> IO ()
generateCode file name tree = do
  -- program header
  hPutStrLn file "@d = internal constant [4 x i8] c\"%d\\0A\\00\""
  hPutStrLn file "declare i32 @printf(i8*, ...)"

  hPutStrLn file "define void @printInt(i32 %x) {"
  hPutStrLn file "%t0 = getelementptr [4 x i8], [4 x i8]* @d, i32 0, i32 0"
  hPutStrLn file "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
  hPutStrLn file "ret void"
  hPutStrLn file "}"

  hPutStrLn file "define i32 @main() nounwind uwtable {"
  hPutStrLn file "entry:"

  -- program contents
  generateProgram file tree

  -- program end
  hPutStrLn file "ret i32 0"
  hPutStrLn file "}"

generateProgram :: Handle -> Program -> IO ()
generateProgram file (Prog ss) = go file ss (0, Map.empty) where
  go file [] _ = return ()
  go file (s:stms) env = (generateStmt file s env) >>= (go file stms)

type Env = (Int, Map.Map String String)

generateStmt :: Handle -> Stmt -> Env -> IO Env
generateStmt file (SAss (Ident id) exp) env = do
  ((nr, env), res) <- generateExp file exp env
  let var_name = "%var_" ++ id 
  env <- case Map.lookup id env of
      Nothing -> do
        hPutStrLn file $ var_name ++ " = alloca i32"
        return $ Map.insert id var_name env
      Just _ -> return env
  hPutStrLn file $ "store i32 " ++ res ++ ", i32* " ++ var_name
  return (nr, env)
generateStmt file (SExp exp) env = do
  (env, res) <- generateExp file exp env
  hPutStrLn file $ "call void @printInt(i32 " ++ res ++ ")"
  return env

generateExp :: Handle -> Exp -> Env -> IO (Env, String)
generateExp file exp env = go file exp env where
  genNode file e1 e2 env oper = do
    (env,       r1) <- go file e1 env
    ((nr, env), r2) <- go file e2 env
    let reg = "%reg_" ++ (show nr)
    hPutStrLn file $ reg ++ " = " ++ oper ++ " i32 " ++ r1 ++ ", " ++ r2
    return ((nr + 1, env), reg)

  go file (ExpAdd e1 e2) env = genNode file e1 e2 env "add"
  go file (ExpMul e1 e2) env = genNode file e1 e2 env "mul"
  go file (ExpSub e1 e2) env = genNode file e1 e2 env "sub"
  go file (ExpDiv e1 e2) env = genNode file e1 e2 env "sdiv"
  go file (ExpLit n) env = return (env, show n)
  go file (ExpVar (Ident id)) (nr, env) = do
    cell <- case Map.lookup id env of
      Just cell -> return cell
      Nothing -> fail $ "Error: variable " ++ id ++ " not defined"
    let res = "%reg_" ++ (show nr)
    hPutStrLn file $ res ++ " = load i32, i32* " ++ cell
    return ((nr + 1, env), res)

main :: IO ()
main = do
  args <- getArgs
  mapM_ runFile args

