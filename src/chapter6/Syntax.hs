module Syntax where

import LLVM.AST (Name)

data Expr
  = Float Double
  | Var Name
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  deriving (Eq, Ord, Show)
