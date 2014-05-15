module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax


binary s t assoc = Ex.Infix (reservedOp s >> return (Op t)) assoc
unary s t = Ex.Prefix (reservedOp s >> return (UnOp t))
comp s t = Ex.Infix (reservedOp s >> return (CondOp t)) Ex.AssocNone

op :: Parser String
op = do
  o <- operator
  return o

alfaBinary s t assoc = Ex.Infix (reservedOp s >> return (Op t)) assoc

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
          binary "-" BinMinus Ex.AssocLeft]]

compops = [[comp "<" Lt,
             comp ">" Gt,
             comp "=" Eq,
             comp "<=" Geq,
             comp ">=" Leq,
             comp "eq" Eq,
             comp "neq" Neq,
             comp "or" Or,
             comp "and" And]]

unops = [[unary "-" UnMinus]
        ,[unary "not" Not]]

true :: Parser Bool
true = do reserved "true"
          return True

false :: Parser Bool
false = do reserved "false"
           return False

number :: Parser Number
number = try (do {i <- integer; return (Integer i)})
      <|> try (do {f <- float; return (Float f)})
      <|> do {h <- hexadecimal; return (Hexa h)}

datum :: Parser Datum
datum = do {n <- number; return (Number n)}
     <|> try (do {t <- true; return (Bool t)})
     <|> try (do {f <- false; return (Bool f)})
     <|> do {s <- Lexer.string; return (String s)}
     <|> do {c <- character; return (Char c)}

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
factor = do {d <- datum; return (Datum d)}
       <|> do {id <- identifier; return(Id id)}
       <|> do {e <- expr; return (Expr e)}

term :: Parser Term
term = do {f <- factor; return (Term f)}
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
expr = do {t <- term; return (TermExpr t)}
    <|> call
--    <|> lambda
    <|> vectorGet

-- lambda :: Parser Expr
-- lambda = do paramsret <- brackets
--             (params:ret) <- colonSep paramsret
--             reserved "->"
--             body <- many $ statement
--             return $ Lambda (decParams params)
--                             (typedec ret)
--                             body

vectorGet :: Parser Expr
vectorGet = do name <- identifier
               reservedOp "'"
               index <- integer
               return $ Get name index

declaration :: Parser Declaration
declaration = try $ do name <- identifier
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
                         return $ DecParam type_ name)
  return params

function :: Parser Declaration
function = do reserved "dec"
              name <- identifier
              args <- decParams
              colon
              ret <- typedec
              reserved "->"
              body <- many $ statement
              dot
              return $ DecFun name args ret (Block [] body)

condition :: Parser Expr
condition = Ex.buildExpressionParser (compops) condition


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
  iterator <- identifier
  body <- many statement
  dot
  return $ Foreach var iterator body

assign :: Parser Statement
assign = do name <- identifier
            reservedOp "<-"
            exp <- expr
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
           def <- option (Clause [] []) defclause
           def <- case def of
             Clause [] [] -> return []
             _ -> return [def]
           dot
           return $ Match key (clauses ++ def)

repeatU :: Parser Statement
repeatU = do reserved "repeat"
             body <- many statement
             reserved "until"
             cond <- condition
             dot
             return $ RepeatU body cond

repeatT :: Parser Statement
repeatT = do reserved "repeat"
             body <- many statement
             id <- (do n <- number
                       return (Right n)
                    <|> do i <- identifier
                           return (Left i))
             reserved "times"
             dot
             return $ RepeatT body id

retst :: Parser Statement
retst = do reserved "return"
           exp <- expr
           dot
           return $ Return exp

incr :: Parser Statement
incr = do reserved "incr"
          id <- identifier
          dot
          return $ Incr id

decr :: Parser Statement
decr = do reserved "decr"
          id <- identifier
          dot
          return $ Decr id

statement :: Parser Statement
statement = assign
         <|> ifst
         <|> match
         <|> foreach
         <|> try repeatU
         <|> repeatT
         <|> retst
         <|> incr
         <|> decr
         <|> do {e <- expr; return (Statement e)}

block :: Parser Block
block = do decs <- many declaration
           sts <- many statement
           return $ Block decs sts

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Block
parseToplevel s = parse (contents block) "<stdin>" s
