module Lib (
  constraintType
           ) where

import MephistoTypes
import MephistoEval
import MephistoTypeCheck
import MephistoParser
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import MephistoUnify

constraintType :: String -> ThrowsError Type
constraintType str = do
  readVal <- (flip evalState (M.empty, [])) (readExpr str)
  (typ, constr) <- constraints "" [] readVal
  case (unify constr) of
    Just x -> return $ subst x typ
    Nothing -> throwError $ NoType 
