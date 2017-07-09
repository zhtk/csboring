module Skellang where

-- Haskell module generated by the BNF converter

import Abslang
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Prog functions  -> failure x


transFunction :: Function -> Result
transFunction x = case x of
  Fun type' id decls insts  -> failure x


transDecl :: Decl -> Result
transDecl x = case x of
  Decl type' id  -> failure x


transType :: Type -> Result
transType x = case x of
  TInt  -> failure x
  TBool  -> failure x
  TStr  -> failure x
  TTab type' n  -> failure x
  TDict type'0 type'  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EAnd exp0 exp  -> failure x
  EOr exp0 exp  -> failure x
  EGte exp0 exp  -> failure x
  EGt exp0 exp  -> failure x
  ELte exp0 exp  -> failure x
  ELt exp0 exp  -> failure x
  ENeq exp0 exp  -> failure x
  EEq exp0 exp  -> failure x
  EAdd exp0 exp  -> failure x
  ESub exp0 exp  -> failure x
  EMul exp0 exp  -> failure x
  EDiv exp0 exp  -> failure x
  ENeg exp  -> failure x
  Call id exps  -> failure x
  EVar id  -> failure x
  EInt n  -> failure x
  ETab id exp  -> failure x
  EStr str  -> failure x
  ETrue  -> failure x
  EFalse  -> failure x


transInst :: Inst -> Result
transInst x = case x of
  IVar type' id  -> failure x
  ISet id exp  -> failure x
  ITSet id exp0 exp  -> failure x
  IIf exp insts  -> failure x
  IIfe exp insts0 insts  -> failure x
  IWhile exp insts  -> failure x
  ICall id exps  -> failure x
  IPrint exp  -> failure x
  IFort id exp0 exp insts  -> failure x
  IFord id exp0 exp insts  -> failure x
  IBlock insts  -> failure x
  IRet exp  -> failure x



