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
import           Data.Data

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

name :: Parser AST.Name
name = (lexeme . try) ident
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
  bindName  = char ':' *> (name <?> "parameter name")

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

literalValue :: Parser AST.LiteralValue
literalValue = label "literal value" $ lexeme $ choice
  [ try $ AST.HexLiteral <$ string "0x" <*> Lexer.hexadecimal
  , try $ AST.IntLiteral <$> Lexer.decimal <* notFollowedBy
    (letterChar <|> symbolChar)
  , try $ AST.RealLiteral <$> Lexer.float <* notFollowedBy letterChar
  , try $ AST.ScientificLiteral <$> Lexer.scientific
  , try $ AST.NullLiteral <$ keyword "null"
  , try $ AST.TrueLiteral <$ keyword "true"
  , try $ AST.FalseLiteral <$ keyword "false"
  ]

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

