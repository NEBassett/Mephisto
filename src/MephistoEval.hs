module MephistoEval
  (
    subst,
    shift,
    outerSubst,
    eval
  ) where

import MephistoTypes
import Control.Monad.Except

shift :: Int -> Term -> Term
shift d tTerm = recurse 0 tTerm
  where recurse :: Int -> Term -> Term
        recurse n term = case term of
                           Var m -> if (m<n) then term else Var $ m+d
                           Abs str expr -> Abs str (recurse (n+1) expr)
                           App expr0 expr1 -> App (recurse n expr0) (recurse n expr1)

subst :: Int -> Term -> Term -> Term
subst var sub term = recurse 0 term
  where recurse n term = case term of
                           Var m -> if (m == (n+var)) then (shift n sub) else term
                           Abs str expr -> Abs str (recurse (n+1) expr)
                           App expr0 expr1 -> App (recurse n expr0) (recurse n expr1)

outerSubst sub term = shift (-1) (subst 0 (shift 1 sub) term)

isVal :: Context -> Term -> Bool
isVal _ (Abs _ _) = True
isVal _ _ = False

evalImpl :: Context -> Term -> ThrowsError Term
evalImpl env (App func arg) = case (func, isVal env func, isVal env arg) of
                                (Abs str expr, _, True) -> return $ outerSubst arg expr
                                (_, _, True) -> do
                                  evaled <- evalImpl env arg
                                  return $ App func evaled
                                (_,_,_) -> do
                                  evaled <- evalImpl env func
                                  return $ App evaled arg

evalImpl env term = throwError $ NoRule term

eval :: Context -> Term -> Term
eval env term = case (evalImpl env term) of
                  Left _ -> term
                  Right val -> val
