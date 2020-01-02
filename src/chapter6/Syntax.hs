module Syntax (
  Name,
  Op(..),
  Expr(..),
  Defn(..),
  Phrase(..),
) where

import LLVM.AST (Name)

data Expr
  = Float Double
  | Var Name
  | Call Name [Expr]
  -- | Function Name [Name] Expr
  -- | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr
  -- | BinaryDef Name [Name] Expr
  -- | UnaryDef Name [Name] Expr
  deriving (Eq, Ord, Show)

data Op
  = Add
  | Sub
  | Mul
  | Lt
  | Gt
  deriving (Eq, Ord, Show)

data Defn
  = Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryDef Name [Name] Expr
  | UnaryDef Name Name Expr
  deriving (Eq, Ord, Show)

data Phrase
  = DefnPhrase Defn
  | ExprPhrase Expr
  deriving (Eq, Ord, Show)
