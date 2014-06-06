module Syntax where

import qualified Data.Vector as V
import Data.String.Utils (replace)

type Identifier = String

type Size = Integer

data BinOp
  = Plus
  | BinMinus
  | Times
  | Divide
  | Mod
  | Get
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

data Datum
  = Integer Integer
  | Float Double
  | Double Double
  | Hexa Integer
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

data Expr
  = Id Identifier
  | Datum Datum
  | FunCall Identifier CallParams
  | Lambda DecParams Type Block
  | VectOp BinOp Expr Expr
  | Parenthesized Expr
  | UnOp UnaryOp Expr
  | Op BinOp Expr Expr
  | CondOp CompareOp Expr Expr
  deriving (Eq, Ord, Show)

type CallParams =  [Expr]

data DecParam =  DecParam Type Identifier
  deriving (Eq, Ord, Show)

type DecParams =  [DecParam]

type Index = Integer

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
  | ElseIf Expr Then [(Expr, Body)]
  | Match Expr [Clause]
  | Foreach Identifier Identifier Body
  | RepeatU Body Expr
  | RepeatT Body (Either Identifier Datum)
  | Return Expr
  | Incr Identifier
  | Decr Identifier
  | Expr Expr
  | Comment
  | Breakpoint
  | Dec Declaration
  deriving (Eq, Ord, Show)

data DataStruct
  = DataStruct Identifier Type [Property] [Operation]
  deriving (Eq, Ord, Show)

type Override = Bool

data Property = Property Identifier Type
  deriving (Eq, Ord, Show)

data Operation = Operation Identifier DecParams Type Override Block
  deriving (Eq, Ord, Show)

data Block = Block [Statement]
  deriving (Eq, Ord, Show)

-- instance Show (Statement) where
--     show (Statement x) = replace "[" "\n[" $ replace "(" "\n(" $ show x
--     show x = show x
