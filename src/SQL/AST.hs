module SQL.AST where
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           Data.Scientific

newtype Name
  = Name { unName :: Text }
  deriving Show

newtype TableName
  = TableName { unTableName :: Name }
  deriving Show

newtype ColumnName
  = ColumnName { unColumnName :: Name }
  deriving Show

data BindParameter
  = IndexedParameter Int
  | NamedParameter Name
  deriving Show

data UnaryOperator
  = Negative
  | Positive
  | BinaryComplement
  | Negate
  deriving Show

data BinaryOperator
  = Concatenation
  | Multiplication
  | Division
  | Modulus
  | Addition
  | Subtraction
  | BinaryLeftShift
  | BinaryRightShift
  | BinaryAnd
  | BinaryOr
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equality
  | Inequality
  | Is
  | IsNot
  | In
  | Like
  | And
  | Or
  deriving Show

data Sign
  = Plus
  | Minus
  deriving Show

data SignedNumber = SignedNumber
  { sign  :: Maybe Sign
  , value :: Int
  } deriving Show

data LiteralValue
  = HexLiteral Int
  | IntLiteral Int
  | RealLiteral Double
  | ScientificLiteral Scientific
  | StringLiteral String
  | NullLiteral
  | TrueLiteral
  | FalseLiteral
  deriving Show

data SelectStmt
  = SelectStmt
  deriving Show

data InExprRhs
  = SelectStmtRhs SelectStmt
  | ExprRhs (NonEmpty Expr)
  deriving Show

data Expr
  = ValueExpr LiteralValue
  | BindExpr BindParameter
  | ColumnExpr (Maybe TableName) ColumnName
  | UnaryOpExpr UnaryOperator Expr
  | BinaryOpExpr Expr BinaryOperator Expr
  | Parens Expr
  | LikeExpr Expr Bool Expr
  | IsNullExpr Expr Bool Expr
  | IsExpr Expr Bool Expr
  | BetweenExpr Expr Bool Expr Expr
  | InExpr Expr Bool InExprRhs
  | ExistsExpr Bool SelectStmt
  | CaseExpr (Maybe Expr) (NonEmpty (Expr, Expr)) (Maybe Expr)
  deriving Show
