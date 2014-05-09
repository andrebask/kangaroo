module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s t assoc = Ex.Infix (reservedOp s >> return (t)) assoc
unary s t = Ex.Prefix (reservedOp s >> return (t))

op :: Parser String
op = do
  o <- operator
  return o

alfaBinary s t assoc = Ex.Infix (reservedOp s >> return (t)) assoc

alfaOp :: Parser String
alfaOp = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [[binary "<-" Arrow Ex.AssocLeft]
        ,[alfaBinary "mod" Mod Ex.AssocLeft]
        ,[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" BinMinus Ex.AssocLeft]
        ,[binary "<" Lt Ex.AssocNone,
          binary ">" Gt Ex.AssocNone,
          binary "=" Eq Ex.AssocNone,
          binary "<=" Geq Ex.AssocNone,
          binary ">=" Leq Ex.AssocNone,
          alfaBinary "eq" Eq Ex.AssocNone,
          alfaBinary "neq" Neq Ex.AssocNone,
          alfaBinary "or" Or Ex.AssocNone,
          alfaBinary "not" Not Ex.AssocNone,
          alfaBinary "and" And Ex.AssocNone]]

unops = [[unary "-" UnMinus]
        ,[unary "not" Not]]

true :: Parser Bool
true = do reserved "true"
          return True

false :: Parser Bool
false = do reserved "false"
           return False

number :: Parser Number
number = try integer
      <|> try float
      <|> try hexadecimal

datum :: Parser Datum
datum = try number
     <|> try character
     <|> try Lexer.string

expr :: Parser Expr
expr =  Ex.buildExpressionParser (unops ++ binops) factor

variable :: Parser Expr
variable = do
  name <- identifier
  reserved "is"

-- Tutorial code
-----------------------------------------------------------------
function :: Parser Expr
function = do
  reserved "dec"
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
