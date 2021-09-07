{-# language NoImplicitPrelude #-}

module TypeCheck where

import CustomPrelude

import Data.Map (Map)
import qualified Data.Map as Map

import AST

data Env = Env
  { envDataTypes :: Map Ident DataType
  , envVars :: Map Ident Type
  }

type TypeError = (SrcLoc, Text)

typeError :: SrcLoc -> Text -> TypeError
typeError = (,)

type TcM = Either TypeError

infer :: Env -> Expr -> TcM Type
infer env = \case
  IntegerLiteral{} -> pure $ TypeConApp "Int" []
  StringLiteral{}  -> pure $ TypeConApp "String" []
  Var loc name ->
    case Map.lookup name (envVars env) of
      Just ty -> pure ty
      Nothing -> Left $ typeError loc $ "Unknown variable " <> name
  Lambda{} ->
    Left $ typeError unknownSrcLoc "Lambda not inferable"
  Placeholder{} ->
    Left $ typeError unknownSrcLoc "Placeholder should be desugared before type checking"
  MethodCall loc receiver name args -> do
    receiverTy <- infer env receiver
    method <- resolveMethod env receiverTy name
    when (length (methodArgTypes method) /= length args) $ do
      Left $ typeError loc $ "expected " <> tshow (length (methodArgTypes method)) <> " arguments, got " <> tshow (length args)
    forM_ (zip args (methodArgTypes method)) $ \(arg, ty) ->
      check env arg ty
    pure (methodResultType method)
  _ ->
    Left $ typeError unknownSrcLoc "Unimplemented"

check :: Env -> Expr -> Type -> TcM ()
check env expr ty =
  case expr of
    Lambda loc args body ->
      case ty of
        FunctionType argTys resultTy -> do
          when (length args /= length argTys) $
            Left $ typeError loc "mismatched number of arguments"
          check (extendEnvVars (Map.fromList (zip args argTys)) env) body resultTy
    _ -> do
      inferred <- infer env expr
      when (inferred /= ty) $
        Left $ typeError (exprSrcLoc expr) $
          "Type mismatch. Expected " <> tshow ty <> ", got " <> tshow inferred
      

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
  }

resolveMethod :: Env -> Type -> Ident -> TcM Method
resolveMethod _ _ _ = Left $ typeError unknownSrcLoc "Unimplemented resolveMethod"
