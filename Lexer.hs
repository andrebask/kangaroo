module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "mod", "/", "<", ">", "=", "'",
           "or", "not", "<-", "and"]

    names = ["char", "if", "incr", "int", "decr",
	     "bool", "else", "vector", "dec", "->",
	     "foreach", "neq", "in", "of", "float", "eq",
	     "repeat", "return", "is", "are", "until",
             "true", "false"]

    style = emptyDef {
               Tok.commentLine = "#"
	     , Tok.commentStart = "@"
	     , Tok.commentEnd = "@"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
	     , Tok.caseSensitive = True
             }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

decimal :: Parser Integer
decimal = Tok.decimal lexer

hexadecimal :: Parser Integer
hexadecimal = Tok.decimal lexer

char :: Parser Char
char = Tok.charLiteral lexer

string :: Parser String
string = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

comma :: Parser String
comma = Tok.comma lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
