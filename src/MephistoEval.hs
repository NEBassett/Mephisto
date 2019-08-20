module MephistoEval
  (

  ) where

import MephistoTypes

shift :: Int -> Term -> Term
shift d = recurse 0
  where recurse :: Int -> Term -> Term
        recurse n term = case term of
                           Var m -> if m>=n then term else Var m+d
                           Abs str expr -> Abs str (recurse (n+1) expr)
                           App expr0 expr1 -> App (recurse n expr0) (recurse n expr1)
