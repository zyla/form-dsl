{-# language NoImplicitPrelude #-}

module Desugar where

import CustomPrelude

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import AST

desugar :: Expr -> Expr
desugar = \case
  expr@IntegerLiteral{} -> expr
  expr@StringLiteral{} -> expr
  expr@Var{} -> expr
  Lambda loc args body -> Lambda loc args (desugar body)
  Placeholder{} -> error "Invalid placeholder"
  expr@(MethodCall loc _ _ _) ->
    case desugarMethodCallSequence expr of
      (Placeholder ploc, replaceReceiver) ->
        Lambda loc ["$x"] $ replaceReceiver (Var ploc "$x")
      (receiver, replaceReceiver) ->
        replaceReceiver receiver

  Binary loc x op y -> Binary loc (desugar x) op (desugar y)
  SequenceLiteral loc items ->
    SequenceLiteral loc (map (dsSequenceItem desugar) items)
  MapLiteral loc items ->
    MapLiteral loc (map (dsSequenceItem (\(k, v) -> (desugar k, desugar v))) items)


  PrimCall{} -> error "PrimCall in desugar"
  RecordAccessor{} -> error "RecordAccessor in desugar"

dsSequenceItem f = \case
  Item x -> Item (f x)
  Splat x -> Splat (desugar x)

desugarMethodCallSequence :: Expr -> (Expr, Expr -> Expr)
desugarMethodCallSequence = \case
  MethodCall loc receiver' name args ->
    let (receiver, replace) = desugarMethodCallSequence receiver'
    in (receiver, \x -> MethodCall loc (replace x) name (map desugar args))
  expr ->
    (desugar expr, \x -> x)
