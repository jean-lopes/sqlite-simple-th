module SQL.AST where
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )

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

data LiteralValue
  = HexLiteral Int
  | IntLiteral Int
  | RealLiteral Double
  | StringLiteral Text
  | NullLiteral
  | TrueLiteral
  | FalseLiteral
  deriving Show

data InExprRhs
  = SelectStmtRhs SelectStmt
  | ExprRhs (NonEmpty Expr)
  deriving Show

data TypeName
  = SimpleType Name [Name]
  | PrecisionType Name [Name] Int
  | ScaleType Name [Name] Int Int
  deriving Show

data Expr
  = ValueExpr LiteralValue
  | BindExpr BindParameter
  | ColumnExpr (Maybe TableName) ColumnName
  | UnaryOpExpr UnaryOperator Expr
  | BinaryOpExpr Expr BinaryOperator Expr
  | Parens (NonEmpty Expr)
  | CastExpr Expr TypeName
  | LikeExpr Expr Expr
  | NotLikeExpr Expr Expr
  | IsNullExpr Expr
  | IsNotNullExpr Expr
  | IsExpr Expr Expr
  | IsNotExpr Expr Expr
  | BetweenExpr Expr Expr Expr
  | NotBetweenExpr Expr Expr Expr
  | InExpr Expr InExprRhs
  | NotInExpr Expr InExprRhs
  | ExistsExpr SelectStmt
  | NotExistsExpr SelectStmt
  | SelectExpr SelectStmt
  | CaseExpr (Maybe Expr) (NonEmpty (Expr, Expr)) (Maybe Expr)
  deriving Show

data ResultColumn
  = All
  | AllFrom TableName
  | ResultColumn Expr
  deriving Show

data InsertStmt
  = InsertDefaultValues TableName [ColumnName]
  | InsertSingleValue TableName [ColumnName] (NonEmpty Expr)
  | InsertMultipleValues TableName [ColumnName] (NonEmpty (NonEmpty Expr))
  deriving Show

data TableOrSubquery
  = Table TableName
  | Subquery SelectStmt
  deriving Show

newtype TablesOrSubqueries
  = TablesOrSubqueries { unTablesOrSubqueries :: NonEmpty TableOrSubquery }
  deriving Show

newtype Where
  = Where { unWhere :: Expr }
  deriving Show

data GroupBy = GroupBy
  { grouped :: NonEmpty Expr
  , having :: Maybe Expr
  } deriving Show

data OrderingTerm
  = Asc Expr
  | Desc Expr
  deriving Show

newtype OrderBy
  = OrderBy { unOrderBy :: NonEmpty OrderingTerm }
  deriving Show

data Limit = Limit
  { limit :: Expr
  , offset :: Maybe Expr
  } deriving Show

data SelectStmt
  = SelectResult (Maybe Bool) (NonEmpty ResultColumn) (Maybe TablesOrSubqueries) (Maybe Where) (Maybe GroupBy) (Maybe OrderBy) (Maybe Limit)
  | SelectValues (NonEmpty (NonEmpty Expr)) (Maybe OrderBy) (Maybe Limit)
  deriving Show

data DeleteStmt = DeleteStmt TableName (Maybe Where)
  deriving Show

data ColumnConstraint
  = NotNull
  | DefaultInt Int
  | DefaultLiteral LiteralValue
  | DefaultExpr Expr
  deriving Show

data ColumnDef = ColumnDef
  { columnName :: ColumnName
  , typeName :: Maybe TypeName
  , constraints :: [ColumnConstraint] -- TODO: use a SET instead
  } deriving Show

data CreateTableStmt = CreateTableStmt
  { table :: TableName
  , columns :: NonEmpty ColumnDef
  } deriving Show

data UpdateStmt = UpdateStmt
  { updateTable :: TableName
  , sets :: NonEmpty (NonEmpty ColumnName, Expr)
  , whereClause :: Maybe Where
  } deriving Show

data SqlStmt
  = Create CreateTableStmt
  | Delete DeleteStmt
  | Insert InsertStmt
  | Select SelectStmt
  | Update UpdateStmt
  deriving Show

newtype SqlStmtList
  = SqlStmtList { unSqlStmtList :: [SqlStmt] }
  deriving Show
