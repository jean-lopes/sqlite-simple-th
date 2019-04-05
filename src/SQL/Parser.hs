{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SQL.Parser where
import           Data.Char
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Set                       ( Set )
import qualified Text.Megaparsec.Char.Lexer    as Lexer
import           Control.Monad                  ( void )
import qualified SQL.AST                       as AST
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Control.Monad.Combinators.Expr
import qualified Control.Applicative.Combinators.NonEmpty
                                               as NonEmpty

data SqlError
  = InvalidIdentifier Text
  deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent SqlError where
  showErrorComponent (InvalidIdentifier msg) =
    "Invalid identifier: " ++ Text.unpack msg

type Parser = Parsec SqlError Text

invalidIdentifier :: Text -> Text -> SqlError
invalidIdentifier ident complement =
  InvalidIdentifier $ Text.concat ["'", ident, "' ", complement]

whitespace :: Parser ()
whitespace = Lexer.space spaceConsumer singleLineComment multilineComment
 where
  spaceConsumer     = void spaceChar
  singleLineComment = Lexer.skipLineComment "--"
  multilineComment  = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: Text -> Parser Text
symbol = Lexer.symbol whitespace

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' whitespace

keyword :: Text -> Parser Text
keyword txt = lexeme $ (string' txt <* notFollowedBy alphaNumChar)

comma :: Parser Text
comma = symbol ","

name :: Parser AST.Name
name = label "identifier" $ (lexeme . try) ident
 where
  rest  = Text.pack <$> many (char '_' <|> alphaNumChar)
  ident = Text.cons <$> letterChar <*> rest >>= \ident ->
    if Text.toUpper ident `Set.member` reserved
      then customFailure $ invalidIdentifier ident "is a reserved word"
      else return $ AST.Name ident

tableName :: Parser AST.TableName
tableName = AST.TableName <$> name

columnName :: Parser AST.ColumnName
columnName = AST.ColumnName <$> name

nat :: Parser Int
nat = read <$> some digitChar <* notFollowedBy letterChar

bindParameter :: Parser AST.BindParameter
bindParameter =
  lexeme
    $   AST.IndexedParameter
    <$> bindIndex
    <|> AST.NamedParameter
    <$> bindName
 where
  bindIndex = char '?' *> (nat <?> "parameter index")
  bindName  = oneOf [':', '@', '$'] *> (name <?> "parameter name")

unaryOperator :: Parser AST.UnaryOperator
unaryOperator = choice
  [ AST.Negative <$ symbol "-"
  , AST.Positive <$ symbol "+"
  , AST.BinaryComplement <$ symbol "~"
  , AST.Negate <$ keyword "not"
  ]

-- TODO: labels / notFollowedBy
binaryOperator :: Parser AST.BinaryOperator
binaryOperator = lexeme $ choice
  [ AST.Concatenation <$ symbol "||"
  , AST.Multiplication <$ symbol "*"
  , AST.Division <$ symbol "/"
  , AST.Modulus <$ symbol "%"
  , AST.Addition <$ symbol "+"
  , AST.Subtraction <$ symbol "-"
  , AST.BinaryLeftShift <$ symbol "<<"
  , AST.BinaryRightShift <$ symbol ">>"
  , AST.BinaryAnd <$ symbol "&"
  , AST.BinaryOr <$ symbol "|"
  , AST.LessThan <$ symbol "<"
  , AST.LessThanOrEqual <$ symbol "<="
  , AST.GreaterThan <$ symbol ">"
  , AST.GreaterThanOrEqual <$ symbol ">="
  , AST.Equality <$ symbol "="
  , AST.Equality <$ symbol "=="
  , AST.Inequality <$ symbol "!="
  , AST.Inequality <$ symbol "<>"
  , AST.Is <$ keyword "is"
  , AST.IsNot <$ keyword "is" <* keyword "not"
  , AST.In <$ keyword "in"
  , AST.Like <$ keyword "like"
  , AST.And <$ keyword "and"
  , AST.Or <$ keyword "or"
  ]

hexLiteral :: Parser AST.LiteralValue
hexLiteral = AST.HexLiteral <$ string "0x" <*> Lexer.hexadecimal

intLiteral :: Parser AST.LiteralValue
intLiteral =
  AST.IntLiteral <$> Lexer.decimal <* notFollowedBy (char '.' <|> letterChar)

realLiteral :: Parser AST.LiteralValue
realLiteral = AST.RealLiteral <$> Lexer.float

nullLiteral :: Parser AST.LiteralValue
nullLiteral = AST.NullLiteral <$ keyword "null"

trueLiteral :: Parser AST.LiteralValue
trueLiteral = AST.TrueLiteral <$ keyword "true"

falseLiteral :: Parser AST.LiteralValue
falseLiteral = AST.FalseLiteral <$ keyword "false"

stringLiteral :: Parser AST.LiteralValue
stringLiteral =
  AST.StringLiteral
    <$  char '\''
    <*> (Text.pack <$> many (norm <|> escaped))
    <*  char '\''
 where
  norm    = satisfy (/= '\'')
  escaped = string "''" *> return '\''

literalValue :: Parser AST.LiteralValue
literalValue = label "literal value" $ lexeme $ choice
  [ try hexLiteral
  , try intLiteral
  , try realLiteral
  , try stringLiteral
  , try nullLiteral
  , try trueLiteral
  , falseLiteral
  ]

qualifiedColumnName :: Parser (Maybe AST.TableName, AST.ColumnName)
qualifiedColumnName =
  (,) <$> optional (try $ tableName <* symbol ".") <*> columnName

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

signedNumber :: Parser Int
signedNumber = Lexer.signed whitespace (lexeme Lexer.decimal)

simpleType :: Parser AST.TypeName
simpleType = AST.SimpleType <$> name <*> many name

precisionType :: Parser AST.TypeName
precisionType =
  AST.PrecisionType <$> name <*> many name <*> parens signedNumber

scaleType :: Parser AST.TypeName
scaleType =
  AST.ScaleType
    <$> name
    <*> many name
    <*  symbol "("
    <*> signedNumber
    <*  symbol ","
    <*> signedNumber
    <*  symbol ")"

typeName :: Parser AST.TypeName
typeName = lexeme $ try scaleType <|> try precisionType <|> simpleType

castExpr :: Parser AST.Expr
castExpr =
  AST.CastExpr
    <$  keyword "cast"
    <*  symbol "("
    <*> expr
    <*  keyword "as"
    <*> typeName
    <*  (symbol ")")

caseExpr :: Parser AST.Expr
caseExpr =
  AST.CaseExpr
    <$  keyword "case"
    <*> optional expr
    <*> NonEmpty.some whenExpr
    <*> optional (keyword "else" *> expr)
    <*  keyword "end"
  where whenExpr = (,) <$ keyword "when" <*> expr <* keyword "then" <*> expr

exists :: Parser AST.Expr
exists =
  AST.ExistsExpr <$ keyword "exists" <* symbol "(" <*> selectStmt <* symbol ")"

notExists :: Parser AST.Expr
notExists =
  AST.NotExistsExpr
    <$  keyword "not"
    <*  keyword "exists"
    <*  symbol "("
    <*> selectStmt
    <*  symbol ")"

selectExpr :: Parser AST.Expr
selectExpr = AST.SelectExpr <$> parens selectStmt

term :: Parser AST.Expr
term = label "term" $ lexeme $ choice
  [ try $ AST.ValueExpr <$> literalValue
  , try $ AST.BindExpr <$> bindParameter
  , try $ uncurry AST.ColumnExpr <$> qualifiedColumnName
  , try $ AST.Parens <$> parens (NonEmpty.some expr)
  , try $ castExpr
  , try $ caseExpr
  , try $ exists
  , try $ notExists
  , selectExpr
  ]

binary name f = InfixL (f <$ symbol name)
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
exprTable = [Postfix (AST.IsNullExpr <$ symbol "isnull")]

expr :: Parser AST.Expr
expr = label "expression" $ lexeme term

resultColumn :: Parser AST.ResultColumn
resultColumn = label "result column" $ lexeme $ choice
  [ try $ AST.All <$ symbol "*"
  , try $ AST.AllFrom <$> tableName <* symbol "." <* symbol "*"
  , AST.ResultColumn <$> expr
  ]

innerValues :: Parser (NonEmpty AST.Expr)
innerValues = symbol "(" *> NonEmpty.sepBy1 expr comma <* symbol ")"

values :: Parser (NonEmpty AST.Expr)
values = lexeme $ keyword "values" *> innerValues

multipleValues :: Parser (NonEmpty (NonEmpty AST.Expr))
multipleValues = lexeme $ keyword "values" *> NonEmpty.sepBy1 innerValues comma

columnNames1 :: Parser [AST.ColumnName]
columnNames1 = option [] (symbol "(" *> sepByComma1 columnName <* symbol ")")

insertDefaultValues :: Parser AST.InsertStmt
insertDefaultValues =
  AST.InsertDefaultValues
    <$  keyword "insert"
    <*  keyword "into"
    <*> tableName
    <*> option [] columnNames1
    <*  keyword "default"
    <*  keyword "values"

insertIntoSingleValue :: Parser AST.InsertStmt
insertIntoSingleValue =
  AST.InsertSingleValue
    <$  keyword "insert"
    <*  keyword "into"
    <*> tableName
    <*> option [] columnNames1
    <*> values

insertIntoMultipleValues :: Parser AST.InsertStmt
insertIntoMultipleValues =
  AST.InsertMultipleValues
    <$  keyword "insert"
    <*  keyword "into"
    <*> tableName
    <*> option [] columnNames1
    <*> multipleValues

insertStmt :: Parser AST.InsertStmt
insertStmt =
  lexeme
    $   try insertDefaultValues
    <|> try insertIntoSingleValue
    <|> insertIntoMultipleValues

sepByComma1 :: Parser a -> Parser [a]
sepByComma1 p = sepBy1 p comma

tableOrSubquery :: Parser AST.TableOrSubquery
tableOrSubquery = lexeme
  $ choice [try $ AST.Subquery <$> parens selectStmt, AST.Table <$> tableName]

tablesOrSubqueries :: Parser AST.TablesOrSubqueries
tablesOrSubqueries =
  lexeme $ AST.TablesOrSubqueries <$> NonEmpty.sepBy1 tableOrSubquery comma

where' :: Parser AST.Where
where' = lexeme $ AST.Where <$ keyword "where" <*> expr

groupBy :: Parser AST.GroupBy
groupBy =
  lexeme
    $   AST.GroupBy
    <$  keyword "group"
    <*  keyword "by"
    <*> NonEmpty.sepBy1 expr comma
    <*> optional (keyword "having" *> expr)

orderingTerm :: Parser AST.OrderingTerm
orderingTerm = label "ordering term" $ lexeme $ try descending <|> ascending
 where
  ascending  = AST.Asc <$> expr <* optional (keyword "asc")
  descending = AST.Desc <$> expr <* keyword "desc"

-- TODO: better error feedback
orderBy :: Parser AST.OrderBy
orderBy =
  lexeme
    $   AST.OrderBy
    <$  keyword "order"
    <*  keyword "by"
    <*> NonEmpty.sepBy1 orderingTerm comma

limit :: Parser AST.Limit
limit =
  lexeme
    $   AST.Limit --
    <$  keyword "limit"
    <*> expr
    <*> optional (offsetOrComma *> expr)
  where offsetOrComma = keyword "offset" <|> comma

selectStmt :: Parser AST.SelectStmt
selectStmt = lexeme $ selectValues <|> selectResult
 where
  selectValues =
    AST.SelectValues --
      <$> multipleValues
      <*> optional orderBy
      <*> optional limit
  selectResult =
    AST.SelectResult
      <$  keyword "select"
      <*> distinctClause
      <*> NonEmpty.sepBy1 resultColumn comma
      <*> optional (keyword "from" *> tablesOrSubqueries)
      <*> optional where'
      <*> optional groupBy
      <*> optional orderBy
      <*> optional limit
  distinctClause =
    optional (True <$ keyword "distinct" <|> False <$ keyword "all")

deleteStmt :: Parser AST.DeleteStmt
deleteStmt =
  lexeme
    $   AST.DeleteStmt
    <$  keyword "delete"
    <*  keyword "from"
    <*> tableName
    <*> optional where'

columnConstraint :: Parser AST.ColumnConstraint
columnConstraint = lexeme $ choice
  [ try $ AST.NotNull <$ keyword "not" <* keyword "null"
  , try $ AST.DefaultInt <$ keyword "default" <*> signedNumber
  , try $ AST.DefaultLiteral <$ keyword "default" <*> literalValue
  , AST.DefaultExpr <$ keyword "default" <* symbol "(" <*> expr <* symbol ")"
  ]

columnDef :: Parser AST.ColumnDef
columnDef =
  lexeme
    $   AST.ColumnDef
    <$> columnName
    <*> optional typeName
    <*> many columnConstraint

createTableStmt :: Parser AST.CreateTableStmt
createTableStmt =
  lexeme
    $   AST.CreateTableStmt
    <$  keyword "create"
    <*  keyword "table"
    <*> tableName
    <*  symbol "("
    <*> NonEmpty.sepBy1 columnDef comma
    <*  symbol ")"

updateStmt :: Parser AST.UpdateStmt
updateStmt =
  lexeme
    $   AST.UpdateStmt
    <$  keyword "update"
    <*> tableName
    <*  keyword "set"
    <*> NonEmpty.sepBy1 setCol comma
    <*> optional where'
 where
  columnList = parens $ NonEmpty.sepBy1 columnName comma
  col        = try columnList <|> return <$> columnName
  setCol     = (,) <$> col <* symbol "=" <*> expr

sqlStmt :: Parser AST.SqlStmt
sqlStmt = lexeme $ choice
  [ try $ AST.Create <$> createTableStmt
  , try $ AST.Delete <$> deleteStmt
  , try $ AST.Insert <$> insertStmt
  , try $ AST.Select <$> selectStmt
  , AST.Update <$> updateStmt
  ]

sqlStmtList :: Parser AST.SqlStmtList
sqlStmtList = lexeme $ AST.SqlStmtList <$> sepBy sqlStmt (symbol ";")

reserved :: Set Text
reserved = Set.fromList
  [ "ABORT"
  , "ACTION"
  , "ADD"
  , "AFTER"
  , "ALL"
  , "ALTER"
  , "ANALYZE"
  , "AND"
  , "AS"
  , "ASC"
  , "ATTACH"
  , "AUTOINCREMENT"
  , "BEFORE"
  , "BEGIN"
  , "BETWEEN"
  , "BY"
  , "CASCADE"
  , "CASE"
  , "CAST"
  , "CHECK"
  , "COLLATE"
  , "COLUMN"
  , "COMMIT"
  , "CONFLICT"
  , "CONSTRAINT"
  , "CREATE"
  , "CROSS"
  , "CURRENT"
  , "CURRENT_DATE"
  , "CURRENT_TIME"
  , "CURRENT_TIMESTAMP"
  , "DATABASE"
  , "DEFAULT"
  , "DEFERRABLE"
  , "DEFERRED"
  , "DELETE"
  , "DESC"
  , "DETACH"
  , "DISTINCT"
  , "DO"
  , "DROP"
  , "EACH"
  , "ELSE"
  , "END"
  , "ESCAPE"
  , "EXCEPT"
  , "EXCLUSIVE"
  , "EXISTS"
  , "EXPLAIN"
  , "FAIL"
  , "FILTER"
  , "FOLLOWING"
  , "FOR"
  , "FOREIGN"
  , "FROM"
  , "FULL"
  , "GLOB"
  , "GROUP"
  , "HAVING"
  , "IF"
  , "IGNORE"
  , "IMMEDIATE"
  , "IN"
  , "INDEX"
  , "INDEXED"
  , "INITIALLY"
  , "INNER"
  , "INSERT"
  , "INSTEAD"
  , "INTERSECT"
  , "INTO"
  , "IS"
  , "ISNULL"
  , "JOIN"
  , "KEY"
  , "LEFT"
  , "LIKE"
  , "LIMIT"
  , "MATCH"
  , "NATURAL"
  , "NO"
  , "NOT"
  , "NOTHING"
  , "NOTNULL"
  , "NULL"
  , "OF"
  , "OFFSET"
  , "ON"
  , "OR"
  , "ORDER"
  , "OUTER"
  , "OVER"
  , "PARTITION"
  , "PLAN"
  , "PRAGMA"
  , "PRECEDING"
  , "PRIMARY"
  , "QUERY"
  , "RAISE"
  , "RANGE"
  , "RECURSIVE"
  , "REFERENCES"
  , "REGEXP"
  , "REINDEX"
  , "RELEASE"
  , "RENAME"
  , "REPLACE"
  , "RESTRICT"
  , "RIGHT"
  , "ROLLBACK"
  , "ROW"
  , "ROWS"
  , "SAVEPOINT"
  , "SELECT"
  , "SET"
  , "TABLE"
  , "TEMP"
  , "TEMPORARY"
  , "THEN"
  , "TO"
  , "TRANSACTION"
  , "TRIGGER"
  , "UNBOUNDED"
  , "UNION"
  , "UNIQUE"
  , "UPDATE"
  , "USING"
  , "VACUUM"
  , "VALUES"
  , "VIEW"
  , "VIRTUAL"
  , "WHEN"
  , "WHERE"
  , "WINDOW"
  , "WITH"
  , "WITHOUT"
  ]

