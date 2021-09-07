module ParserSpec where

import CustomPrelude

import qualified Data.Text as Text
import Text.Show.Pretty (ppShow)

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

import qualified Text.Megaparsec as MP
import qualified Parser

spec :: Spec
spec = do
  let parseTest input =
        it (Text.unpack input) $
          case MP.parse (Parser.expr <* MP.eof) "" input of
            Left err ->
              error $ MP.errorBundlePretty err
            Right expr ->
              defaultGolden (Text.unpack input) (ppShow expr)

  parseTest "1"
  parseTest "\"foo\""
  parseTest "hello"
  parseTest "[1, 2   ,3]"
