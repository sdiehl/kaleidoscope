module Parser where


import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax
import LLVM.AST.Name (Name, mkName)

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp (mkName s))) assoc

name :: Parser Name
name = mkName <$> identifier

op :: Parser Name
op = do
  whitespace
  o <- operator
  whitespace
  return (mkName o)

binops =
  [ [ binary "*" Ex.AssocLeft,
      binary "/" Ex.AssocLeft
    ],
    [ binary "+" Ex.AssocLeft,
      binary "-" Ex.AssocLeft
    ],
    [binary "<" Ex.AssocLeft]
  ]

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> name

function :: Parser Defn
function = do
  reserved "def"
  nm <- name
  args <- parens $ many name
  body <- expr
  return $ Function nm args body

extern :: Parser Defn
extern = do
  reserved "extern"
  nm <- name
  args <- parens $ many name
  return $ Extern nm args

call :: Parser Expr
call = do
  nm <- name
  args <- parens $ commaSep expr
  return $ Call nm args

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
  var <- name
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

unarydef :: Parser Defn
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens name
  body <- expr
  return $ UnaryDef o args body

binarydef :: Parser Defn
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many name
  body <- expr
  return $ BinaryDef o args body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> for
      <|> (parens expr)

defn :: Parser Defn
defn = try extern
    <|> try function
    <|> try unarydef
    <|> try binarydef

phrase :: Parser Phrase
phrase = (DefnPhrase <$> defn) <|> (ExprPhrase <$> expr)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Phrase]
toplevel = many $ do
  def <- phrase
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Phrase]
parseToplevel s = parse (contents toplevel) "<stdin>" s
