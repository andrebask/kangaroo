module Syntax where

type Identifier = String

type Size = Int

data BinOp
  = Plus
  | BinMinus
  | Times
  | Divide
  | Mod
  | And
  | Or
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
  = Int
  | Float
  | Double
  deriving (Eq, Ord, Show)

data Datum
  = Number
  | Bool
  | Char
  | String
  | List
  | Vector
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
  = Identifier
  | Datum
  | Expr
  | Parenthesized Expr
  deriving (Eq, Ord, Show)

data Term
  = Factor
  | UnaryOp Term
  | BinOp Term Term
  deriving (Eq, Ord, Show)

type CallParams =  [Term]

data DecParam =  DecParam Type Identifier
  deriving (Eq, Ord, Show)

type DecParams =  [DecParam]

type Index = Identifier

data Expr
  = Term
  | FunCall Identifier CallParams
  | Lambda DecParams Type Block
  | Get Identifier Index
  deriving (Eq, Ord, Show)

data Condition = Op CompareOp Expr Expr
  deriving (Eq, Ord, Show)

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
  | If Condition Then Else
  | Match Expr [Clause]
  | Foreach Identifier Identifier Body
  | RepeatU Body Condition
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
