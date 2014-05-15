module Syntax where

import qualified Data.Vector as V

type Identifier = String

type Size = Integer

data BinOp
  = Plus
  | BinMinus
  | Times
  | Divide
  | Mod
  | Arrow
  | CompareOp
  deriving (Eq, Ord, Show)

data CompareOp
  = Gt
  | Lt
  | Geq
  | Leq
  | Eq
  | Neq
  | And
  | Or
  deriving (Eq, Ord, Show)

data UnaryOp
  = UnMinus
  | Not
  deriving (Eq, Ord, Show)

data Comment
  = LineComment
  | BlockComment
  deriving (Eq, Ord, Show)

data Number
  = Integer Integer
  | Float Double
  | Double Double
  | Hexa Integer
  deriving (Eq, Ord, Show)

data Datum
  = Number Number
  | Bool Bool
  | Char Char
  | String String
  | List [Datum]
  | Vector (V.Vector Datum)
  deriving (Eq, Ord, Show)

data Type
  = IntType
  | BoolType
  | CharType
  | StringType
  | FloatType
  | VoidType
  | VectType Size Type
  deriving (Eq, Ord, Show)

data Factor
  = Id Identifier
  | Datum Datum
  | Expr Expr
  | Parenthesized Expr
  deriving (Eq, Ord, Show)

data Term
  = Term Factor
  | UnOp UnaryOp Term
  | Op BinOp Term Term
  deriving (Eq, Ord, Show)

type CallParams =  [Term]

data DecParam =  DecParam Type Identifier
  deriving (Eq, Ord, Show)

type DecParams =  [DecParam]

type Index = Integer

data Expr
  = TermExpr Term
  | CondOp CompareOp Expr Expr
  | FunCall Identifier CallParams
  | Lambda DecParams Type Block
  | Get Identifier Index
  deriving (Eq, Ord, Show)

-- type Condition = CondOp CompareOp Expr Expr
--  deriving (Eq, Ord, Show)

type Body = [Statement]
type Then = Body
type Else = Body

data Clause = Clause [Datum] Body
  deriving (Eq, Ord, Show)

data Declaration
  = DecVar Identifier Type
  | DecVect Identifier Size Type
  | DecFun Identifier DecParams Type Block
  | DecStruct DataStruct
  deriving (Eq, Ord, Show)

data Statement
  = Assign Identifier Expr
  | If Expr Then Else
  | Match Expr [Clause]
  | Foreach Identifier Identifier Body
  | RepeatU Body Expr
  | RepeatT Body (Either Identifier Number)
  | Return Expr
  | Incr Identifier
  | Decr Identifier
  | Statement Expr
  | Comment
  | Breakpoint
  deriving (Eq, Ord, Show)

data DataStruct
  = DataStruct Identifier Type [Property] [Operation]
  deriving (Eq, Ord, Show)

type Override = Bool

data Property = Property Identifier Type
  deriving (Eq, Ord, Show)

data Operation = Operation Identifier DecParams Type Override Block
  deriving (Eq, Ord, Show)

data Block = Block [Declaration] [Statement]
  deriving (Eq, Ord, Show)
