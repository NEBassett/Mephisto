module MephistoEval
  (
    shift,
    outerSubst,
    eval
  ) where

import MephistoTypes
import Control.Monad.Except

isVal :: Context -> Term -> Bool
isVal _ (Abs _ _ _) = True
isVal _ TTrue = True
isVal _ TFalse = True
isVal _ _ = False

evalImpl :: Context -> Term -> ThrowsError Term
evalImpl env (If a b c) = do
  pred <- evalImpl env a
  case pred of
    TTrue -> evalImpl env b
    TFalse -> evalImpl env c
    m -> throwError $ BadPredicate m
evalImpl env (App func arg) = case (func, isVal env func, isVal env arg) of
                                (Abs _ str expr, _, True) -> return $ outerSubst arg expr
                                (_, _, True) -> do
                                  evaled <- evalImpl env arg
                                  return $ App func evaled
                                (_,_,_) -> do
                                  evaled <- evalImpl env func
                                  return $ App evaled arg

evalImpl env term = throwError $ NoRule term

eval :: Context -> Term -> ThrowsError Term
eval env term = case (evalImpl env term) of
                  Left m -> case m of
                    NoRule t -> return t
                    n -> throwError n
                  Right val -> eval env val
