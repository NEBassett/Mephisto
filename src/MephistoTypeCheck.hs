module MephistoTypeCheck
  (
    typeof,
    constraints
  ) where

import MephistoTypes
import Control.Monad.Except
import qualified Control.Lens as L

constraints :: String -> Context -> Term -> ThrowsError (Type, [Constraint])
constraints str env (Var n) = (getType env n) >>= (\m -> return (m, []))
constraints str env (Abs typ name term) = do
  (resType, constr) <- constraints str ([(name, Variable typ)] ++ env) term
  return (Func typ resType, constr)
constraints str env (App func arg) = do
  (funcType, constr0) <- constraints ('a':str) env func
  (argType, constr1) <- constraints ('b':str) env arg
  return (TVar ('c':str), constr0 ++ constr1 ++ [Constraint funcType (Func argType (TVar ('c':str)))])
constraints str env (If a b c) = do
  (aType, constr0) <- constraints ('a':str) env a
  (bType, constr1) <- constraints ('b':str) env b
  (cType, constr2) <- constraints ('c':str) env c
  return (bType, constr0 ++ constr1 ++ constr2 ++ [
             Constraint aType (Base MBool),
             Constraint bType cType
                                                  ])
constraints str env (TTuple terms) =
    (func 0 terms) >>= (\(types, constr) -> return (MTuple types, constr))
  where
    func ind [l] = (constraints (str ++ (show ind)) env l) >>= (\(typ, constr) -> return ([typ], constr))
    func ind (l:ls) = (do
                          (typ, constr) <- constraints (str ++ (show ind)) env l
                          (typN, constrN) <- func (ind+1) ls
                          return (([typ] ++ typN), constr++constrN))
constraints _ _ TTrue = return (Base MBool, [])
constraints _ _ TFalse = return (Base MBool, [])

  

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
