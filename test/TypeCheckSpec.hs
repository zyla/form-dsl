module TypeCheckSpec where

import CustomPrelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Show.Pretty (ppShow)
import qualified Data.Map as Map

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

import qualified Text.Megaparsec as MP
import qualified Parser
import qualified TypeCheck as TC
import AST (DataType(..), Type, Expr)
import Utils (parseType)
import Desugar (desugar)

import System.Directory (createDirectoryIfMissing, doesFileExist)

env :: TC.Env
env = TC.Env
  { TC.envDataTypes = Map.fromList
    [ ("Product", RecordType { dtFields =
        [ ("name", parseType "Translated<String>")
        , ("photo", parseType "Photo")
        , ("price", parseType "Money")
        , ("parameters", parseType "Array<ParameterId>")
        ]
      , dtIdType = Just $ parseType "ProductId"
      })
    , ("MenuCatalogue", RecordType { dtFields =
        [ ("products", parseType "Map<ProductId, Product>")
        ]
        , dtIdType = Nothing })
    ]
  , TC.envVars = Map.fromList
    [ ("catalogue", parseType "MenuCatalogue")
    , ("product", parseType "Product")
    , ("products", parseType "Array<Product>")
    , ("productWithId", parseType "WithId<Product>")
    ]
  }

spec :: Spec
spec = do
  let tcTest input ty =
        it (Text.unpack (input <> ": " <> ty)) $
          let expr = desugar (parseExpr input) in
          case TC.runTcM $ TC.infer env expr of
            Right (elaborated, ty') -> do
              parseType ty `shouldBe` ty'
              runGolden input (Text.pack $ ppShow elaborated)
            Left err ->
              error $ "Type error: " <> show err

  tcTest "1" "Int"
  tcTest "\"foo\"" "String"
  tcTest "product" "Product"

  tcTest "product.name" "Translated<String>"
  tcTest "products.map(_.name)" "Array<Translated<String>>"
  tcTest "products.flatMap(p => products)" "Array<Product>"
  tcTest "products.length" "Int"
  tcTest "products.map(x => products).map(_.length)" "Array<Int>"

  tcTest "productWithId.id" "ProductId"
  tcTest "productWithId.name" "Translated<String>"


parseExpr :: Text -> Expr
parseExpr input =
  case MP.parse (Parser.expr <* MP.eof) "" input of
    Left err ->
      error $ MP.errorBundlePretty err
    Right x ->
      x

-- Taken from Test.Hspec.Golden
runGolden :: Text -> Text -> IO ()
runGolden name actual = do
  let actualName = Text.unpack $ ".golden/" <> name <> "/actual"
  let goldenName = Text.unpack $ ".golden/" <> name <> "/golden"

  createDirectoryIfMissing True $ Text.unpack $ ".golden/" <> name
  goldenExists <- doesFileExist goldenName

  Text.writeFile actualName actual

  if goldenExists then do
    golden <- Text.readFile goldenName
    actual `shouldBe` golden
  else
    Text.writeFile goldenName actual
