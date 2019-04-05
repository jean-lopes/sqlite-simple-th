{-# LANGUAGE OverloadedStrings #-}
module Main where
import           SQL.Parser
import qualified SQL.AST                       as AST
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text                      ( Text )

test :: Parser a -> Text -> Either (ParseErrorBundle Text SqlError) a
test p = parse (p <* eof) ""
main :: IO ()
main = hspec $ describe "Parser" $ it "nat" $ test nat "1" `shouldParse` 1
