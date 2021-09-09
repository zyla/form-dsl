{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import CustomPrelude

import AST (Type(..))
import qualified Parser
import qualified Text.Megaparsec as MP

parseType :: Text -> Type
parseType input =
  case MP.parse (Parser.type_ <* MP.eof) "" input of
    Left err ->
      error $ MP.errorBundlePretty err
    Right x ->
      x
