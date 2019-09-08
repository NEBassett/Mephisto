module MephistoUnify
  (
    Constraint(..),
    unify,
    subst
  ) where

import MephistoTypes
import Data.Maybe

subst :: [(Type, Type)] -> Type -> Type
subst zs val@(TVar s) = fromMaybe val $ lookup val zs 
subst zs (Func m n) = Func (subst zs m) (subst zs n)
subst zs (MTuple ms) = MTuple (fmap (subst zs) ms)
subst zs (Sum m n) = Sum (subst zs m) (subst zs n)
subst zs m = m

csubst :: [(Type, Type)] ->  Constraint -> Constraint
csubst zs (Constraint m n) = Constraint (subst zs m) (subst zs n)

unify :: [Constraint] -> Maybe [(Type, Type)]
unify [] = Just []
unify ((Constraint s u):cs) =
  if (s == u)
  then (unify cs) 
  else if ((isJust varS) && (not (isInFv (fromJust varS) u)))
  then ((:) (s,u) <$> (unify $ fmap (csubst [(s,u)]) cs))
  else if ((isJust varU) && (not (isInFv (fromJust varU) s)))
  then ((:) (u,s) <$> (unify $ fmap (csubst [(u,s)]) cs))
  else if ((isFunc s) && (isFunc u))
  then (unify $ [Constraint (inputType s) (inputType u), Constraint (outputType s) (outputType u)] ++ cs)
  else Nothing 
  where varS = isVar s
        varU = isVar u
        isFunc (Func t y) = True
        isFunc _ = False
        inputType (Func t _) = t
        outputType (Func _ y) = y
        isVar (TVar t) = Just t
        isVar _ = Nothing
                                      
                                      
