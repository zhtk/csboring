{-# LANGUAGE DeriveDataTypeable #-}

module Abslang where

import Data.Data
import Data.Dynamic
import Data.Map

data Stored =
   SInt Integer
 | SBool Bool
 | SStr String
 | STab Type Integer (Map Integer Stored)
 | SDict Type Type (Map Stored Stored)
 | SFun Function
  deriving (Eq,Ord,Show,Data,Typeable)

type Store = Map Int Stored
type Env = Map String Int
type InterState = (Env, Store, Int)

newtype Ident = Ident String deriving (Eq,Ord,Show,Data,Typeable)
data Program =
   Prog [Function]
  deriving (Eq,Ord,Show,Data,Typeable)

data Function =
   Fun Type Ident [Decl] [Inst]
  deriving (Eq,Ord,Show,Data,Typeable)

data Decl =
   Decl Type Ident
  deriving (Eq,Ord,Show,Data,Typeable)

data Type =
   TInt
 | TBool
 | TStr
 | TTab Type Integer
 | TDict Type Type
  deriving (Eq,Ord,Show,Data,Typeable)

data Exp =
   EAnd Exp Exp
 | EOr Exp Exp
 | EGte Exp Exp
 | EGt Exp Exp
 | ELte Exp Exp
 | ELt Exp Exp
 | ENeq Exp Exp
 | EEq Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | ENeg Exp
 | Call Ident [Exp]
 | EVar Ident
 | EInt Integer
 | ETab Ident Exp
 | EStr String
 | ETrue
 | EFalse
  deriving (Eq,Ord,Show,Data,Typeable)

data Inst =
   IVar Type Ident
 | ISet Ident Exp
 | ITSet Ident Exp Exp
 | IIf Exp [Inst]
 | IIfe Exp [Inst] [Inst]
 | IWhile Exp [Inst]
 | ICall Ident [Exp]
 | IPrint Exp
 | IFort Ident Exp Exp [Inst]
 | IFord Ident Exp Exp [Inst]
 | IBlock [Inst]
 | IRet Exp
  deriving (Eq,Ord,Show,Data,Typeable)

