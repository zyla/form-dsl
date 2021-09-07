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
  | MethodCall SrcLoc Expr Ident [Expr]
  | Binary SrcLoc Expr Operator Expr
  | SequenceLiteral SrcLoc [SequenceItem Expr]
  | MapLiteral SrcLoc [SequenceItem (Expr, Expr)]
  deriving (Eq, Show)

data SequenceItem a = Item a | Splat Expr
  deriving (Eq, Show)
