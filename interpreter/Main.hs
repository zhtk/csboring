module Main where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Dynamic
import Data.Maybe
import Data.Map
import System.Environment
import System.IO

import Lexlang
import Parlang
import ErrM
import Abslang
import Static
import Exec

insertFunToEnv :: InterState -> Function -> InterState
insertFunToEnv (env, store, alloc) fun@(Fun _ (Ident id) _ _) = (
  insert id alloc env,
  insert alloc (SFun fun) store,
  alloc + 1)

executeProgram :: Program -> IO ()
executeProgram (Prog functions) = do
  -- Wgranie do środowiska funkcji
  let (env, store, alloc) = Prelude.foldl insertFunToEnv (empty, empty, 0) functions
  
  -- Static check
  let check = staticCheck (env, store, alloc) functions
  case check of
    -- Program execution
    Ok () -> case runFunction (env, store, alloc) "main" [] of
      Ok _ -> return ()
      Bad s -> hPutStrLn stderr s
    Bad s -> hPutStrLn stderr s

main :: IO ()
main = do
  args <- getArgs
  if length args > 1 then
    hPutStrLn stderr "Invalid number of arguments. Usage: ./interpreter file"
  else if length args == 1 then do
    let [file] = args
    hFile <- openFile file ReadMode
    program <- hGetContents hFile
    let tree = pProgram $ myLexer program
    case tree of
      Bad e -> hPutStrLn stderr $ "Error during parsing: " ++ e
      Ok tree -> executeProgram tree
  else do
    program <- hGetContents stdin
    let tree = pProgram $ myLexer program
    case tree of
      Bad e -> hPutStrLn stderr $ "Error during parsing: " ++ e
      Ok tree -> executeProgram tree
