{-# language NoImplicitPrelude #-}

module TypeCheck where

import CustomPrelude

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Trans.State (StateT, runStateT, modify, gets)
import Control.Monad.Trans.Class (lift)

import AST
import Utils (parseType)

data Env = Env
  { envDataTypes :: Map Ident DataType
  , envVars :: Map Ident Type
  }

type TypeError = (SrcLoc, Text)

typeError :: SrcLoc -> Text -> TypeError
typeError = (,)

type TcM = StateT TcState (Either TypeError)

runTcM :: TcM a -> Either TypeError a
runTcM = fmap fst . flip runStateT (TcState mempty)

data TcState = TcState
  { tcSubst :: Map Ident Type
  }

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
      Nothing -> err $ typeError loc $ "Unknown variable " <> name
  Lambda{} ->
    err $ typeError unknownSrcLoc "Lambda not inferable"
  Placeholder{} ->
    err $ typeError unknownSrcLoc "Placeholder should be desugared before type checking"
  MethodCall loc receiver name args -> do
    (eReceiver, receiverTy) <- infer env receiver
    method <- resolveMethod env loc receiverTy name
    when (length (methodArgTypes method) /= length args) $ do
      err $ typeError loc $ "expected " <> tshow (length (methodArgTypes method)) <> " arguments, got " <> tshow (length args)
    eArgs <- forM (zip args (methodArgTypes method)) $ \(arg, ty) ->
      check env arg ty
    s <- gets tcSubst
    pure (methodCompileCall method eReceiver eArgs, subst s (methodResultType method))
  _ ->
    err $ typeError unknownSrcLoc "Unimplemented"

-- | Check expression against a given type, and elaborate it.
check :: Env -> Expr -> Type -> TcM Expr
check env expr ty =
  case expr of
    Lambda loc args body ->
      case ty of
        FunctionType argTys resultTy -> do
          when (length args /= length argTys) $
            err $ typeError loc "mismatched number of arguments"
          eBody <- check (extendEnvVars (Map.fromList (zip args argTys)) env) body resultTy
          pure $ Lambda loc args eBody
        _ ->
          err $ typeError loc $
            "Type mismatch. Expected " <> tshow ty <> ", got a function"
    _ -> do
      (elaborated, inferred) <- infer env expr
      unify (exprSrcLoc expr) inferred ty
      pure elaborated

unify :: SrcLoc -> Type -> Type -> TcM ()
unify _ (TypeVar v) ty =
  -- TODO: check if not already resolved!
  modifySubst (Map.insert v ty)
unify loc ty (TypeVar v) = unify loc (TypeVar v) ty
unify loc (TypeConApp ty1 args1) (TypeConApp ty2 args2) = do
  when (ty1 /= ty2) $
    err $ typeError loc $ "Type mismatch: " <> ty1 <> " /= " <> ty2
  when (length args1 /= length args2) $
    err $ typeError loc $ "Mismatched number of generic type arguments: " <> tshow (length args1) <> " /= " <> tshow (length args2)
  forM_ (zip args1 args2) $ \(t1, t2) -> unify loc t1 t2

modifySubst :: (Map Ident Type -> Map Ident Type) -> TcM ()
modifySubst f = modify (\s -> s { tcSubst = f (tcSubst s) })

err :: TypeError -> TcM a
err = lift . Left

findDataType :: Env -> Ident -> TcM DataType
findDataType env name =
  case Map.lookup name (envDataTypes env) of
    Just ty -> pure ty
    Nothing -> err $ typeError unknownSrcLoc $ "Unknown data type " <> name

extendEnvVars :: Map Ident Type -> Env -> Env
extendEnvVars vars env = env { envVars = vars <> envVars env }


data Method =
  Method
  { methodArgTypes :: [Type]
  , methodResultType :: Type
  , methodCompileCall :: Expr -> [Expr] -> Expr
  }
  

resolveMethod :: Env -> SrcLoc -> Type -> Ident -> TcM Method
resolveMethod env loc receiverType name =
  case receiverType of
    -- type WithId entity = { id :: IdType entity, entity :: entity }
    TypeConApp "WithId" [entityType@(TypeConApp dtName [])] ->
      case name of
        "id" -> do
          dt <- findDataType env dtName
          idType <- case dtIdType dt of
            Just t -> pure t
            Nothing -> err $ typeError loc $ "No ID type specified for " <> dtName
          pure $ Method
            { methodArgTypes = []
            , methodResultType = idType
            , methodCompileCall = \receiver _ -> RecordAccessor loc receiver "id"
            }

        _ -> do
          method <- resolveMethod env loc entityType name
          pure $ method { methodCompileCall = \receiver args -> methodCompileCall method (RecordAccessor loc receiver "entity") args }

    TypeConApp dtName tyArgs | Just primType <- Map.lookup dtName primTypes -> do
      method <- case Map.lookup name (primTypeMethods primType) of
        Just m -> pure m
        Nothing ->
          err $ typeError loc $ "Method " <> name <> " not found on type " <> dtName
      let tyvars = Map.fromList $ zip (primTypeParameters primType) tyArgs
      let primName = "prim_" <> dtName <> "_" <> name
      pure $ Method
        { methodArgTypes = subst tyvars <$> primMethodArgs method
        , methodResultType = subst tyvars (primMethodReult method)
        , methodCompileCall = \receiver args -> PrimCall loc primName (receiver : args)
        }
    -- Property accessor for a record type
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
          err $ typeError loc $ "Property " <> name <> " not found on type " <> dtName
    _ ->
      err $ typeError loc "Unimplemented case in resolveMethod"

subst :: Map Ident Type -> Type -> Type
subst s = \case
  TypeVar v ->
    fromMaybe (TypeVar v) $ Map.lookup v s
  TypeConApp name args ->
    TypeConApp name (subst s <$> args)
  FunctionType params ret ->
    FunctionType (subst s <$> params) (subst s ret)

data PrimMethod = PrimMethod
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
      [ ("map", PrimMethod ["b"] [parseType "(a) => b"] (parseType "Array<b>"))
      , ("flatMap", PrimMethod ["b"] [parseType "(a) => Array<b>"] (parseType "Array<b>"))
      , ("length", PrimMethod [] [] (parseType "Int"))
      ])
  ]

-- xs :: Array<Dynamic<Int>>
-- xs.map(x => y => x + y) : Dynamic<Array<(Int) => Int>>
--
-- observable(() => xs.map(x => y => x() + y()))
-- 
