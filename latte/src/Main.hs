module Main where

import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import LexLatte
import ParLatte
import SkelLatte
import AbsLatte
import Static
import Compile

import ErrM

programError :: String -> IO ()
programError msg = do
  hPutStrLn stderr "ERROR"
  hPutStrLn stderr msg
  exitFailure

ioErrorHandler :: IOError -> IO String
ioErrorHandler err = do
  programError $ show err
  return ""

parseProgram :: String -> IO (Program (Maybe (Int, Int)))
parseProgram code =
  let ts = myLexer code in
  case pProgram ts of
    Bad s -> do
      programError s
      return $ Program Nothing []
    Ok tree -> return tree

main :: IO ()
main = do
  args <- getArgs
  file <- case args of
    [f] -> return f
    _ -> do
      programError "Usage: latc file.lat"
      return ""
  code <- readFile file `catchIOError` ioErrorHandler
  tree <- parseProgram code
  doStaticCheck tree `catchIOError` (programError . ioeGetErrorString)
  let name = (dropExtension file) ++ ".ll"
  handle <- openFile name WriteMode
  compileLatte handle tree
  hFlush handle
  hClose handle
  cmd <- runCommand $ "llvm-as " ++ name
  waitForProcess cmd
  hPutStrLn stderr "OK"
