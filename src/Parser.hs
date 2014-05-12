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
      <|> hexadecimal

datum :: Parser Datum
datum = number
     <|> character
     <|> try true
     <|> try false
     <|> Lexer.string

typedec :: Parser Type
typedec = do reserved "int"
             return IntType
       <|> do reserved "bool"
              return BoolType
       <|> do reserved "char"
              return CharType
       <|> do reserved "string"
              return StringType
       <|> do reserved "float"
              return FloatType

factor :: Parser Factor
factor = datum
       <|> identifier
       <|> expr

term :: Parser Term
term = factor
    <|> Ex.buildExpressionParser (unops ++ binops) term

-- expr :: Parser Expr
-- expr =  Ex.buildExpressionParser (unops ++ binops) factor

callParams :: Parser CallParams
callParams = many $ do whitespace
                       t <- term
                       return t

call :: Parser Expr
call = do
  name <- identifier
  args <- callParams
  return $ FunCall name args

expr :: Parser Expr
expr = term
    <|> call
    <|> lambda
    <|> vectorGet

lambda :: Parser Expr
lambda = do paramsret <- brackets
            (params:ret) <- colonSep paramsret
            reserved "->"
            body <- many $ statement
            return $ Lambda (decParams params)
                            (typedec ret)
                            body

vectorGet :: Parser Expr
vectorGet = do name <- identifier
               reservedOp "'"
               index <- integer
               return $ Get name index

dec :: Parser Declaration
dec = try $ do name <- identifier
               reserved "is"
               type_ <- typedec
               return $ DecVar name type_
   <|> do name <- identifier
          reserved "is"
          reserved "vector"
          reserved "of"
          size <- integer
          type_ <- typedec
          return $ DecVect name size type_
   <|> function
--   <|> structure --TODO

decParams :: Parser DecParams
decParams = do
  whitespace
  params <- commaSep (do type_ <- typedec
                         name <- identifier
                         return DecParam type_ name)
  return params

function :: Parser Declaration
function = do reserved "dec"
              name <- identifier
              args <- many decParams
              colon
              ret <- typedec
              reserved "->"
              body <- many $ statement
              dot
              return $ DecFun name args ret body

condition :: Parser Condition
condition = 0 --TODO

ifthenelse :: Parser Statement
ifthenelse = do
  reserved "if"
  cond <- condition
  comma
  th <- many statement
  reserved "else"
  el <- many statement
  dot
  return $ If cond th el

ifthen :: Parser Statement
ifthen = do
  reserved "if"
  cond <- condition
  comma
  th <- many statement
  dot
  return $ If cond th []

ifst :: Parser Statement
ifst = try ifthenelse <|> ifthen

foreach :: Parser Statement
foreach = do
  reserved "foreach"
  var <- identifier
  reserved "in"
  iterator <-identifier
  body <- many statement
  dot
  return $ Foreach var iterator body

assign :: Parser Statement
assign = do name <- identifier
            reservedOp "<-"
            epx <- expr
            dot
            return $ Assign name exp

clause :: Parser Clause
clause = do datums <- commaSep1 datum
            comma
            body <- many statement
            return $ Clause datums body

defclause :: Parser Clause
defclause = do comma
               body <- many statement
               return $ Clause [] body

match :: Parser Statement
match = do reserved "match"
           key <- expr
           colon
           clauses <- many clause
           def <- option [] defclause
           dot
           return $ Match key (clauses ++ def)

repeatU :: Parser Statement
repeatU = 0 --TODO

repeatT :: Parser Statement
repeatT = 0 --TODO

statement :: Parser Statement
statement = 0 --TODO

-- Tutorial code
-----------------------------------------------------------------

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
