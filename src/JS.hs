module JS where

import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

type Ident = Text

data Expr
  = Function (Maybe Ident) [Ident] Expr
  | Apply Expr [Expr]
  | Var Ident
  | String Text
  | Num Integer
  | ArrayLit [Expr]
  | If Expr Expr Expr
  | BlockExpr [Statement]
  deriving (Eq, Show)

data Statement
  = StatementExpression Expr      -- e;
  | ReturnStatement Expr          -- return e;
  | VariableDefinition Text Expr  -- var v = e;
  deriving (Eq, Show)

renderExpr :: Expr -> Text
renderExpr = \case
  Function ident args body ->
    "(" <> fromMaybe "" ident <> "(" <> T.intercalate "," args <> ") => " <> renderExpr body <> ")"

  Var ident ->
    ident

  Num x ->
    T.pack (show x)

  String x ->
    T.pack (show x)

  ArrayLit xs ->
    "[" <> T.intercalate "," (map renderExpr xs) <> "]"

  Apply fn args ->
    renderExpr fn <> "(" <> T.intercalate "," (map renderExpr args) <> ")"

  If cond then_ else_ ->
    "(" <> renderExpr cond <> ")?(" <> renderExpr then_ <> "):(" <> renderExpr else_ <> ")"

  BlockExpr stmts ->
    "((()=>{" <> foldMap renderStmt stmts <> "})())"

renderStmt :: Statement -> Text
renderStmt = \case
  StatementExpression expr ->
    renderExpr expr <> ";"
  ReturnStatement expr ->
    "return " <> renderExpr expr <> ";"
  VariableDefinition ident expr ->
    "var " <> ident <> " = " <> renderExpr expr <> ";"
