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
    BoundTVar -> return $ TVar ind

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
  return $ Func typ (typeShift (-1) 0 resultType)
typeof env (App func arg) = do
  funcType <- typeof env func
  argType <- typeof env arg
  case funcType of
    Func a b -> if (argType == a) then (return a) else (throwError $ BadType a argType)
    val -> throwError $ BadTypeMsg val "arrow type"
typeof env (TAbs name term) = do
  resultType <- typeof ([(name, BoundTVar)] ++ env) term
  return $ All name resultType
typeof env (TApp func tArg) = do
  funcType <- typeof env func
  case funcType of
    All _ ty -> return $ outerTypeSubst tArg funcType
    val -> throwError $ BadTypeMsg val "universal type"
typeof env (Pack ty0 term ty1) = case ty1 of
                                    Some name ty -> do
                                      termType <- typeof env term
                                      let subbed = outerTypeSubst ty0 ty in
                                        if (subbed == termType)
                                        then (return ty1)
                                        else (throwError $ BadTypeMsg termType  "proper existential pack")
                                    val -> throwError $ BadTypeMsg val "existential type"
typeof env (Unpack name0 name1 sub term) = do
  subType <- typeof env sub
  case subType of
    Some name ty -> do
      resultType <- typeof ([(name1, Variable ty), (name0, BoundTVar)] ++ env) term
      return $ typeShift (-2) 0 resultType
    val -> throwError $ BadTypeMsg val "existential type"

  
