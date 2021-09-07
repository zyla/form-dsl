{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import CustomPrelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST (Expr(..), Ident, Operator, SequenceItem(..), unknownSrcLoc)

type Parser = Parsec Void Text

{- Taken from https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html -}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

{- End taken from -}

-- Taken from https://hackage.haskell.org/package/megaparsec-6.5.0/docs/Text-Megaparsec-Char-Lexer.html#v:charLiteral
stringLiteral :: Parser Text
stringLiteral = lexeme $ char '"' *> (Text.pack <$> (manyTill L.charLiteral (char '"')))

expr :: Parser Expr
expr = methodCallExpr

methodCallExpr :: Parser Expr
methodCallExpr = do
  receiver <- primaryExpr
  chain <- many $ do
    symbol "."
    name <- identifier 
    args <- optional $ between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
    pure (unknownSrcLoc, name, fromMaybe [] args)
  pure $ foldl (\r (loc, name, args) -> MethodCall loc r name args) receiver chain

primaryExpr :: Parser Expr
primaryExpr
  =   IntegerLiteral unknownSrcLoc <$> integer
  <|> StringLiteral unknownSrcLoc <$> stringLiteral
  <|> Lambda unknownSrcLoc <$> try (parameterList <* symbol "=>") <*> expr
  <|> Var unknownSrcLoc <$> identifier
  <|> between (symbol "(") (symbol ")") expr
  <|> SequenceLiteral unknownSrcLoc <$> between (symbol "[") (symbol "]") (sequenceItem expr `sepBy` symbol ",")
  <|> MapLiteral unknownSrcLoc <$> between (symbol "{") (symbol "}") (sequenceItem mapEntry `sepBy` symbol ",")
  <|> Placeholder unknownSrcLoc <$ symbol "_"

parameterList :: Parser [Ident]
parameterList
  =   pure <$> identifier
  <|> between (symbol "(") (symbol ")") (identifier `sepBy` symbol ",")

sequenceItem :: Parser a -> Parser (SequenceItem a)
sequenceItem inner
  =   Splat <$> (symbol "..." *> expr)
  <|> Item <$> inner

mapEntry :: Parser (Expr, Expr)
mapEntry = (,) <$> expr <*> (symbol ":" *> expr)

identifier :: Parser Ident
identifier = lexeme $ Text.pack <$> ((:) <$> satisfy isIdentifierStart <*> many (satisfy isIdentifierChar))
  where
  isIdentifierStart c = Char.isAlphaNum c
  isIdentifierChar c = isIdentifierStart c || Text.any (==c) "$_"

operator :: Parser Operator
operator = lexeme $ Text.pack <$> ((:) <$> satisfy isOperatorChar <*> many (satisfy isOperatorChar))

isOperatorChar :: Char -> Bool
isOperatorChar c = Text.any (==c) "~!@#$%^&*_+-=,./?:<>"
  || (c >= '\x1f000' && c <= '\x1ffff')
