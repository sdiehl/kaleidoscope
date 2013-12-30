--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Syntax where

type Name = String

data Expr
  = Int Integer
  | Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)
