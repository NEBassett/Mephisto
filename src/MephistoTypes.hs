module MephistoTypes
  (
    Term(..),
    Context(..),
    ThrowsError(..),
    MError(..)
  ) where

import qualified Data.Map as M
import qualified Control.Lens as L
import Control.Monad.Except

data Bind = Bound

type Context = [(String, Bind)] -- its nice to have this as a list

data Term = Var Int -- De Bruijn index
          | Abs String Term
          | App Term Term

data MError = NoRule Term

type ThrowsError = Either MError

instance Show Term where show = (crawlingShow [])

crawlingShow :: Context -> Term -> String
crawlingShow env (Abs hint term) =
  let (name, val) = getName env (hint, Bound)
      newEnv = [(name, val)] ++ env
  in "(lambda " ++  name ++ ". " ++ (crawlingShow newEnv term) ++ ")"

crawlingShow env (Var ind) = maybe ("bad index: " ++ show ind) (\a -> a) (fmap fst (env  L.^? (L.element ind)))

crawlingShow env (App function argument) = "(" ++ (crawlingShow env function) ++ " " ++ (crawlingShow env argument) ++ ")"

getName :: Context -> (String, Bind) -> (String, Bind)
getName env str = impl env str 0
  where impl env (str, val) n = if (any ((== str) . fst) env) then (impl env ((str ++ (show n), val)) (n+1)) else (str, val)
