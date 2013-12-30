--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = Int <$> integer

floating :: Parser Expr
floating = Float <$> float

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

letins :: Parser Expr
letins = do
  reserved "var"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ UnaryDef o args body

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many identifier
  body <- expr
  return $ BinaryDef o args body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> try letins
      <|> for
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
