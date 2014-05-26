module Operators where

import Syntax
import Lexer

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import qualified Text.Parsec.Expr as Ex
import qualified Data.Map as M

binop = Ex.Infix (Op <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnOp <$> uop)
compop = Ex.Infix (CondOp <$> cop) Ex.AssocLeft

binary s t assoc = Ex.Infix (reservedOp s >> return (Op t)) assoc
unary s t = Ex.Prefix (reservedOp s >> return (UnOp t))
comp s t = Ex.Infix (reservedOp s >> return (CondOp t)) Ex.AssocNone

opMap = M.fromList [("+",   Plus)
                   ,("*",   Times)
                   ,("-",   BinMinus)
                   ,("mod", Mod)
                   ,("/",   Divide)
                   ,("<-",  Arrow)]

unopMap = M.fromList [("not",   Not)
                   ,("-",   UnMinus)]

compMap = M.fromList [("<",   Lt)
                     ,(">",   Gt)
                     ,("<=",  Leq)
                     ,(">=",  Geq)
                     ,("=",   Eq)
                     ,("eq",  Eq)
                     ,("neq", Neq)
                     ,("or",  Or)
                     ,("and", And)]

opLookup key = M.lookup key opMap
unopLookup key = M.lookup key unopMap
compLookup key = M.lookup key compMap

op :: Parser BinOp
op = try (
  do whitespace
     o <- operator
     whitespace
     case (opLookup o) of
       Nothing -> unexpected "operator"
       Just op -> return op)

uop :: Parser UnaryOp
uop = try (
  do whitespace
     o <- operator
     whitespace
     case (unopLookup o) of
       Nothing -> unexpected "operator"
       Just op -> return op)

cop :: Parser CompareOp
cop = try (
  do whitespace
     o <- operator
     whitespace
     case (compLookup o) of
       Nothing -> unexpected "operator"
       Just op -> return op)

alphaBinary s t assoc = Ex.Infix (reservedOp s >> return (Op t)) assoc

binops = [[binary "<-" Arrow Ex.AssocLeft]
        ,[alphaBinary "mod" Mod Ex.AssocLeft]
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
