module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Control.Monad.Trans
import System.Console.Haskeline

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax
import Operators

end :: Parser String
end = do dot

true :: Parser Bool
true = do reserved "true"
          return True

false :: Parser Bool
false = do reserved "false"
           return False

number :: Parser Datum
number = try (do {f <- float; return (Float f)})
      <|> try (do {i <- integer; return (Integer i)})
      <|> do {h <- hexadecimal; return (Hexa h)}

datum :: Parser Datum
datum = try (do {n <- number; return n})
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

factor :: Parser Expr
factor =  try call
      <|> do {d <- datum; return (Datum d)}
      <|> do {id <- identifier; return(Id id)}
--    <|> lambda

expr :: Parser Expr
expr = Ex.buildExpressionParser (unops ++ binops ++ vectops ++ [[unop],[binop],[vectop]]) factor
    <|> do {f <- factor; return f}

callParams :: Parser CallParams
callParams = many (whitespace >> expr)

call :: Parser Expr
call = do
  name <- identifier
  colon
  args <- callParams
  return $ FunCall name args

-- lambda :: Parser Expr
-- lambda = do paramsret <- brackets
--             (params:ret) <- colonSep paramsret
--             reserved "->"
--             body <- many $ statement
--             return $ Lambda (decParams params)
--                             (typedec ret)
--                             body

-- vectorGet :: Parser Expr
-- vectorGet = do vect <- expr
--                char '\\'
--                index <- expr
--                return $ VectOp Get vect index

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
              end
              return $ DecFun name args ret (Block [] body)

condition :: Parser Expr
condition = Ex.buildExpressionParser (compops ++ [[compop]]) factor


ifthenelse :: Parser Statement
ifthenelse = do
  reserved "if"
  cond <- condition
  comma
  th <- many statement
  reserved "else"
  el <- many statement
  end
  return $ If cond th el

ifthen :: Parser Statement
ifthen = do
  reserved "if"
  cond <- condition
  comma
  th <- many statement
  end
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
  end
  return $ Foreach var iterator body

assign :: Parser Statement
assign = do name <- identifier
            reservedOp "<-"
            exp <- expr
            end
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
           end
           return $ Match key (clauses ++ def)

repeatU :: Parser Statement
repeatU = do reserved "repeat"
             body <- many statement
             reserved "until"
             cond <- condition
             end
             return $ RepeatU body cond

repeatT :: Parser Statement
repeatT = do reserved "repeat"
             body <- many statement
             reserved "for"
             id <- (do n <- number
                       return (Right n)
                    <|> do i <- identifier
                           return (Left i))
             reserved "times"
             end
             return $ RepeatT body id

retst :: Parser Statement
retst = do reserved "return"
           exp <- expr
           end
           return $ Return exp

incr :: Parser Statement
incr = do reserved "incr"
          id <- identifier
          end
          return $ Incr id

decr :: Parser Statement
decr = do reserved "decr"
          id <- identifier
          end
          return $ Decr id

statement :: Parser Statement
statement =  ifst
         <|> match
         <|> foreach
         <|> try repeatU
         <|> repeatT
         <|> retst
         <|> incr
         <|> decr
         <|> try assign
         <|> do {e <- expr; end; return (Statement e)}

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
