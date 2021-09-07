{-# LANGUAGE OverloadedStrings #-}

module Parser where

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
expr = optional sc *> expr'

expr' :: Parser Expr
expr'
  =   IntegerLiteral unknownSrcLoc <$> integer
  <|> StringLiteral unknownSrcLoc <$> stringLiteral
  <|> Var unknownSrcLoc <$> identifier
  <|> SequenceLiteral unknownSrcLoc <$> between (symbol "[") (symbol "]") (sequenceItem expr' `sepBy` symbol ",")
  <|> MapLiteral unknownSrcLoc <$> between (symbol "{") (symbol "}") (many (sequenceItem mapEntry))

sequenceItem :: Parser a -> Parser (SequenceItem a)
sequenceItem inner
  =   Splat <$> (symbol "..." *> expr')
  <|> Item <$> inner

mapEntry :: Parser (Expr, Expr)
mapEntry = (,) <$> expr' <*> (symbol ":" *> expr')

identifier :: Parser Operator
identifier = lexeme $ Text.pack <$> ((:) <$> satisfy isIdentifierStart <*> many (satisfy isIdentifierChar))
  where
  isIdentifierStart c = Char.isAlphaNum c
  isIdentifierChar c = isIdentifierStart c || Text.any (==c) "$_"

operator :: Parser Operator
operator = lexeme $ Text.pack <$> ((:) <$> satisfy isOperatorChar <*> many (satisfy isOperatorChar))

isOperatorChar :: Char -> Bool
isOperatorChar c = Text.any (==c) "~!@#$%^&*_+-=,./?:<>"
  || (c >= '\x1f000' && c <= '\x1ffff')
