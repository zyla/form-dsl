{-# language NoImplicitPrelude #-}

module TypeCheck where

import CustomPrelude

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import AST

data Env = Env
  { envDataTypes :: Map Ident DataType
  , envVars :: Map Ident Type
  }

type TypeError = (SrcLoc, Text)

typeError :: SrcLoc -> Text -> TypeError
typeError = (,)

type TcM = Either TypeError

-- | Infer type of an expression and elaborate it.
infer :: Env -> Expr -> TcM (Expr, Type)
infer env expr = case expr of
  IntegerLiteral{} ->
    pure (expr, TypeConApp "Int" [])
  StringLiteral{}  ->
    pure (expr, TypeConApp "String" [])
  Var loc name ->
    case Map.lookup name (envVars env) of
      Just ty -> pure (expr, ty)
      Nothing -> Left $ typeError loc $ "Unknown variable " <> name
  Lambda{} ->
    Left $ typeError unknownSrcLoc "Lambda not inferable"
  Placeholder{} ->
    Left $ typeError unknownSrcLoc "Placeholder should be desugared before type checking"
  MethodCall loc receiver name args -> do
    (eReceiver, receiverTy) <- infer env receiver
    method <- resolveMethod env loc receiverTy name
    when (length (methodArgTypes method) /= length args) $ do
      Left $ typeError loc $ "expected " <> tshow (length (methodArgTypes method)) <> " arguments, got " <> tshow (length args)
    eArgs <- forM (zip args (methodArgTypes method)) $ \(arg, ty) ->
      check env arg ty
    pure (methodCompileCall method eReceiver eArgs, methodResultType method)
  _ ->
    Left $ typeError unknownSrcLoc "Unimplemented"

-- | Check expression against a given type, and elaborate it.
check :: Env -> Expr -> Type -> TcM Expr
check env expr ty =
  case expr of
    Lambda loc args body ->
      case ty of
        FunctionType argTys resultTy -> do
          when (length args /= length argTys) $
            Left $ typeError loc "mismatched number of arguments"
          eBody <- check (extendEnvVars (Map.fromList (zip args argTys)) env) body resultTy
          pure $ Lambda loc args eBody
        _ ->
          Left $ typeError loc $
            "Type mismatch. Expected " <> tshow ty <> ", got a function"
    _ -> do
      (elaborated, inferred) <- infer env expr
      when (inferred /= ty) $
        Left $ typeError (exprSrcLoc expr) $
          "Type mismatch. Expected " <> tshow ty <> ", got " <> tshow inferred
      pure elaborated

findDataType :: Env -> Ident -> TcM DataType
findDataType env name =
  case Map.lookup name (envDataTypes env) of
    Just ty -> pure ty
    Nothing -> Left $ typeError unknownSrcLoc $ "Unknown data type " <> name

extendEnvVars :: Map Ident Type -> Env -> Env
extendEnvVars vars env = env { envVars = vars <> envVars env }


data Method =
  Method
  { methodArgTypes :: [Type]
  , methodResultType :: Type
  , methodCompileCall :: Expr -> [Expr] -> Expr
  }

resolveMethod :: Env -> SrcLoc -> Type -> Ident -> TcM Method
resolveMethod env loc receiverType name = do
  -- First, try a simple property accessor
  case receiverType of
    TypeConApp dtName [] -> do
      dt <- findDataType env dtName
      case List.lookup name (dtFields dt) of
        Just ty ->
          pure $ Method
            { methodArgTypes = []
            , methodResultType = ty
            , methodCompileCall = \receiver _ -> 
                RecordAccessor loc receiver name
            }
        Nothing ->
          Left $ typeError loc $ "Property " <> name <> " not found on type " <> dtName
    _ ->
      Left $ typeError loc "Unimplemented case in resolveMethod"

data PrimMetod = PrimMethod
  { primMethodTypeParameters :: [Ident]
  , primMethodArgs :: [Type]
  , primMethodReult :: Type
  }

data PrimType = PrimType
  { primTypeParameters :: [Ident]
  , primTypeMethods :: Map Ident PrimMethod
  }

primTypes :: Map Ident PrimType
primTypes = Map.fromList
  [ ("Array", PrimType ["a"] $ Map.fromList
      [ ("map", PrimMethod ["b"] (parseType "(a) => b") (parseType "b"))
      ])
  ]
