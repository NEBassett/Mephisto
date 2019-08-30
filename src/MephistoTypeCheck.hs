module MephistoTypeCheck
  (
    typeof
  ) where

import MephistoTypes
import Control.Monad.Except
import qualified Control.Lens as L

getType :: Context -> Int -> ThrowsError Type
getType env ind = case (fmap snd (env L.^? (L.element ind))) of
  Nothing -> throwError $ NoType
  Just val -> case val of
    Bound -> throwError $ NoType
    Variable typ -> return $ typ

typeof :: Context -> Term -> ThrowsError Type
typeof env TTrue = return $ (Base MBool)
typeof env TFalse = return $ (Base MBool)
typeof env (If a b c) = do
  anteType <- typeof env a
  case anteType of
    (Base MBool) -> do
      consType <- typeof env b
      elseType <- typeof env c
      if (consType == elseType) then (return consType) else (throwError $ BadType consType elseType)
    typ -> throwError $ BadType (Base MBool) typ
typeof env (Var n) = getType env n
typeof env (Abs typ name term) = do
  resultType <- typeof ([(name, Variable typ)] ++ env) term
  return $ Func typ resultType
typeof env (App func arg) = do
  funcType <- typeof env func
  argType <- typeof env arg
  case funcType of
    Func a b -> if (argType == a) then (return a) else (throwError $ BadType a argType)
    val -> throwError $ BadTypeMsg val "arrow type"
