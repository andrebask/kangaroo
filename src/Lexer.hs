module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Char (space, oneOf, alphaNum)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "mod", "/",
           "<", ">", "<=", ">=", "=",
           "eq", "neq", "'", "or", "not", "<-",
           "and"]

    names = ["char", "if", "incr", "int", "decr",
	     "bool", "else", "vector", "dec", "->",
	     "foreach", "in", "of", "float",
	     "repeat", "return", "is", "are", "until",
             "true", "false"]

    style = emptyDef {
               Tok.commentLine = "#"
	     , Tok.commentStart = "@"
	     , Tok.commentEnd = "@"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
	     , Tok.caseSensitive = True
             , Tok.identLetter	 = alphaNum <|> oneOf "_"
             }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

hexadecimal :: Parser Integer
hexadecimal = Tok.hexadecimal lexer

character :: Parser Char
character = Tok.charLiteral lexer

string :: Parser String
string = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

colonSep :: Parser a -> Parser [a]
colonSep p = sepBy p colon

spaceSep :: Parser a -> Parser [a]
spaceSep p = sepBy p space

comma :: Parser String
comma = Tok.comma lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

operator :: Parser String
operator = Tok.operator lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
