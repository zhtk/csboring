module TypeCheck2 where

import IntLambda
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Error
import Data.Maybe (fromJust)

type Env = Map.Map Name Type
type TCM a = ErrorT String (Reader Env) a

envType :: Name -> TCM Type
envType n = do
  Just t <- asks $ Map.lookup n
  return t

runTCM :: TCM a -> Either String a
runTCM = runEnvReader . runErrorT where
  runEnvReader :: Reader (Map.Map k t) a -> a
  runEnvReader r = runReader r Map.empty

typeOf :: Exp -> Either String Type
typeOf exp = runTCM $ m exp where
  m exp = typeOfM exp `catchError` h
  h e = throwError $ e

typeCheck :: Exp -> IO ()
typeCheck exp =
  case typeOf exp of
    Left s -> putStrLn s
    Right t -> print t

typeOfM :: Exp -> TCM Type
typeOfM (EInt _) = return TInt
typeOfM (ELam n t e) = do
  -- TODO
  return TInt
typeOfM (EVar n) = envType n
typeOfM (EApp e e') = return TInt -- TODO
