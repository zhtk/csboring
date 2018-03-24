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

-- Depth of stack for full tree, structure for left and right subtree
data ExpStack = StackLeaf Int | StackNode Int ExpStack ExpStack

uncurry_stack (StackLeaf n) = n
uncurry_stack (StackNode n _ _) = n

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
             let out = (dropExtension f) ++ ".j"
             file <- openFile out WriteMode
             generateCode file (takeBaseName f) tree
             hFlush file
             hClose file
             cmd <- runCommand $ "java -jar lib/jasmin.jar -d " ++ (takeDirectory f) ++ " " ++ out
             _ <- waitForProcess cmd
             return ()

programStackUsage :: Program -> Int
programStackUsage (Prog stms) = maximum (map statementStackUsage stms)

statementStackUsage :: Stmt -> Int
statementStackUsage (SAss _ exp) = uncurry_stack $ expressionStackUsage exp
statementStackUsage (SExp exp) = (uncurry_stack $ expressionStackUsage exp) + 1

optimizeExpStack :: Exp -> Exp -> ExpStack
optimizeExpStack e1 e2 = let
  s1 = expressionStackUsage e1
  s2 = expressionStackUsage e2
  n1 = uncurry_stack s1
  n2 = uncurry_stack s2
  stack = if n1 < n2 then n2 else max n1 (n2 + 1)
  in StackNode stack s1 s2

expressionStackUsage :: Exp -> ExpStack
expressionStackUsage (ExpAdd e1 e2) = optimizeExpStack e1 e2
expressionStackUsage (ExpSub e1 e2) = optimizeExpStack e1 e2
expressionStackUsage (ExpMul e1 e2) = optimizeExpStack e1 e2
expressionStackUsage (ExpDiv e1 e2) = optimizeExpStack e1 e2
expressionStackUsage (ExpLit _) = StackLeaf 1
expressionStackUsage (ExpVar _) = StackLeaf 1

programVariablesUsage :: Program -> Either String Int
programVariablesUsage (Prog sts) = go sts Map.empty where
  go :: [Stmt] -> Map.Map String () -> Either String Int
  go (s:ss) env = do
    env2 <- checkStatement s env
    go ss env2
  go [] env = return $ Map.size env

  checkStatement :: Stmt -> Map.Map String () -> Either String (Map.Map String ())
  checkStatement (SExp exp) env = do
    checkExp exp env
    Right env
  checkStatement (SAss (Ident id) exp) env = do
    checkExp exp env
    Right $ Map.insert id () env

  checkExp :: Exp -> Map.Map String () -> Either String ()
  checkExp (ExpAdd e1 e2) env = (checkExp e1 env) >> (checkExp e2 env)
  checkExp (ExpSub e1 e2) env = (checkExp e1 env) >> (checkExp e2 env)
  checkExp (ExpMul e1 e2) env = (checkExp e1 env) >> (checkExp e2 env)
  checkExp (ExpDiv e1 e2) env = (checkExp e1 env) >> (checkExp e2 env)
  checkExp (ExpLit _) _ = Right ()
  checkExp (ExpVar (Ident id)) env = case Map.lookup id env of
    Nothing -> Left $ "Error: undefined variable " ++ id
    Just _ -> Right ()

generateCode :: Handle -> String -> Program -> IO ()
generateCode file name tree = do
  -- Stack size and type check
  locals <- case programVariablesUsage tree of
    Left msg -> fail msg
    Right n -> return n
  let stack_size = programStackUsage tree

  putStrLn $ "[Locals usage] " ++ (show locals)
  putStrLn $ "[Stack usage] " ++ (show stack_size)

  -- program header
  hPutStrLn file $ ".class public " ++ name
  hPutStrLn file ".super java/lang/Object"

  hPutStrLn file ".method public <init>()V"
  hPutStrLn file "  aload_0"
  hPutStrLn file "  invokespecial java/lang/Object/<init>()V"
  hPutStrLn file "  return"
  hPutStrLn file ".end method"

  hPutStrLn file ".method public static main([Ljava/lang/String;)V"
  hPutStrLn file $ ".limit stack " ++ (show stack_size)
  hPutStrLn file $ ".limit locals " ++ (show $ locals + 1)

  -- program contents
  generateProgram file tree

  -- program end
  hPutStrLn file "return"
  hPutStrLn file ".end method"

generateProgram :: Handle -> Program -> IO ()
generateProgram file (Prog ss) = go file ss (0, Map.empty) where
  go file [] _ = return ()
  go file (s:stms) env = (generateStmt file s env) >>= (go file stms)

generateStmt :: Handle -> Stmt -> (Int, Map.Map String Int) -> IO (Int, Map.Map String Int)
generateStmt file (SAss (Ident id) exp) (nr, env) = do
  generateExp file exp (nr, env)
  let (write, new_env) = case Map.lookup id env of {
      Nothing -> (nr, (nr + 1, Map.insert id nr env)) ;
      Just n -> (n, (nr, env))
  }
  hPutStrLn file $ "istore " ++ (show write)
  return new_env
generateStmt file (SExp exp) env = do
  hPutStrLn file "getstatic java/lang/System/out Ljava/io/PrintStream;"
  generateExp file exp env
  hPutStrLn file "invokevirtual java/io/PrintStream/println(I)V"
  return env

generateExp :: Handle -> Exp -> (Int, Map.Map String Int) -> IO ()
generateExp file exp env = go file exp env (expressionStackUsage exp) where
  genNode file e1 e2 env (StackNode _ sl sr) swap oper = do
    let stackleft = uncurry_stack sl
    let stackright = uncurry_stack sr
    if stackleft < stackright
      then do
        go file e2 env sr
        go file e1 env sl
        if swap then hPutStrLn file "swap" else return ()
      else do
        go file e1 env sl
        go file e2 env sr
    hPutStrLn file oper
    return ()

  go file (ExpAdd e1 e2) env stack = genNode file e1 e2 env stack False "iadd"
  go file (ExpMul e1 e2) env stack = genNode file e1 e2 env stack False "imul"
  go file (ExpSub e1 e2) env stack = genNode file e1 e2 env stack True "isub"
  go file (ExpDiv e1 e2) env stack = genNode file e1 e2 env stack True "idiv"
  go file (ExpLit n) _ _ = hPutStrLn file $ "bipush " ++ (show n)
  go file (ExpVar (Ident id)) (_, env) _ = case Map.lookup id env of
    Just cell -> hPutStrLn file $ "iload " ++ (show cell)
    Nothing -> fail $ "Error during location of variable " ++ id

main :: IO ()
main = do
  args <- getArgs
  mapM_ runFile args

