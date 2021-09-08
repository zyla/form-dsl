{-# language NoImplicitPrelude #-}

module AST where

import CustomPrelude

type Ident = Text
type Operator = Text

type SrcLoc = () -- TODO

unknownSrcLoc :: SrcLoc
unknownSrcLoc = ()

data Expr
  = IntegerLiteral SrcLoc Integer
  | StringLiteral SrcLoc Text
  | Var SrcLoc Ident
  | Lambda SrcLoc [Ident] Expr
  | Apply SrcLoc Expr [Expr]
  | Placeholder SrcLoc
  | MethodCall SrcLoc Expr Ident [Expr]
  | Binary SrcLoc Expr Operator Expr
  | SequenceLiteral SrcLoc [SequenceItem Expr]
  | MapLiteral SrcLoc [SequenceItem (Expr, Expr)]

  -- only after elaboration:
  | PrimCall SrcLoc Ident [Expr]
  | RecordAccessor SrcLoc Expr Ident
  deriving (Eq, Show)

data SequenceItem a = Item a | Splat Expr
  deriving (Eq, Show)

exprSrcLoc :: Expr -> SrcLoc
exprSrcLoc = \case
  IntegerLiteral loc _ -> loc
  StringLiteral loc _ -> loc
  Var loc _ -> loc
  Lambda loc _ _ -> loc
  Placeholder loc -> loc
  MethodCall loc _ _ _ -> loc
  Binary loc _ _ _ -> loc
  SequenceLiteral loc _ -> loc
  MapLiteral loc _ -> loc
  PrimCall loc _ _ -> loc
  RecordAccessor loc _ _ -> loc

data Type
  = TypeVar Ident
  | TypeConApp Ident [Type]
  | FunctionType [Type] Type
  deriving (Eq, Show)

data DataType =
  RecordType
  { dtFields :: [(Ident, Type)]
  }
