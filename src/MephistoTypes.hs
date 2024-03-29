module MephistoTypes
  (
    Term(..),
    Context(..),
    ThrowsError(..),
    MError(..),
    Type(..),
    Bind(..),
    Atomic(..),
    Constraint(..),
    isInFv
  ) where


import qualified Data.Map as M
import qualified Control.Lens as L
import Data.List
import Control.Monad.Except
import Text.Parsec

data Constraint = Constraint Type Type

data Atomic = MBool | MDouble | MString | MNat deriving (Eq)

data Type = Base Atomic | Func Type Type | MTuple [Type] | Sum Type Type | TVar String

data Bind = Bound | Variable Type

type Context = [(String, Bind)] -- its nice to have this as a list

data Term = Var Int -- De Bruijn index
          | Abs Type String Term
          | App Term Term
          | If Term Term Term
          | TTrue
          | TFalse
          | TTuple [Term]
          

data MError = NoRule Term
            | BadPredicate Term
            | NoType
            | BadType Type Type
            | BadTypeMsg Type String
            | Parser ParseError

type ThrowsError = Either MError

instance Show Term where show = (crawlingShow [])
instance Show Type where show = showt
instance Show MError where show = showe
instance Show Atomic where show = showa
instance Show Constraint where show = showc
instance Eq Type where (==) = eq

eq :: Type -> Type -> Bool
eq (TVar s) (TVar d) = s == d
eq (Base a) (Base b) = a == b
eq (Func a b) (Func c d) = (eq a c) && (eq b d)
eq (MTuple a) (MTuple b) = fst $ foldr (\m (n, (l:ls)) -> (((eq m l) && n), ls)) (True, b) a
eq (Sum a b) (Sum c d) = (eq a c) && (eq b d)
eq _ _ = False

crawlingShow :: Context -> Term -> String
crawlingShow env (Abs typ hint term) =
  let (name, val) = getName env (hint, Variable typ)
      newEnv = [(name, val)] ++ env
  in "(lambda " ++  name ++ ". " ++ (crawlingShow newEnv term) ++ ")"

crawlingShow env (Var ind) = maybe ("bad index: " ++ show ind) (\a -> a) (fmap fst (env  L.^? (L.element ind)))

crawlingShow env (App function argument) = "(" ++ (crawlingShow env function) ++ " " ++ (crawlingShow env argument) ++ ")"
crawlingShow env TTrue = "true"
crawlingShow env TFalse = "false"
crawlingShow env (TTuple ms) = "(" ++ (intercalate ", " (fmap (crawlingShow env) ms)) ++ ")" 
crawlingShow env (If a b c) = "If " ++ (crawlingShow env a) ++ " then " ++ (crawlingShow env b) ++ " else " ++ (crawlingShow env c)

showa :: Atomic -> String
showa MBool = "Bool"
showa MNat = "Natural"
showa MDouble = "Double"
showa MString = "String" 

showc :: Constraint -> String
showc (Constraint a b) = (show a) ++ " = " ++ (show b)

showt :: Type -> String
showt (Base a) = showa a
showt (Func a b) = "(" ++ (showt a) ++ " -> " ++ (showt b) ++ ")"
showt (MTuple ms) = "(" ++ (intercalate ", " (fmap showt ms)) ++ ")"
showt (Sum a b) = (showt a) ++ " + " ++ (showt b)
showt (TVar s) = s 

showe :: MError -> String
showe (NoRule m) = "No evaluation rule for: " ++ (show m)
showe (BadPredicate m) = "Bad predicate: " ++ (show m)
showe NoType = "Untyped term"
showe (BadType m n) = "Bad type, expected: " ++ (show m) ++ ", got: " ++ (show n)
showe (BadTypeMsg m s) = "Bad type, expected: " ++ s ++ ", got: " ++ (show m)
showe (Parser m) = "Parser error: " ++ (show m)

getName :: Context -> (String, Bind) -> (String, Bind)
getName env str = impl env str 0
  where impl env (str, val) n = if (any ((== str) . fst) env) then (impl env ((str ++ (show n), val)) (n+1)) else (str, val)

isInFv :: String -> Type -> Bool
isInFv s (Base _) = False
isInFv s (Func m n) = (isInFv s m) || (isInFv s n)
isInFv s (MTuple ms) = foldr ((||) . (isInFv s)) False ms
isInFv s (Sum m n) = (isInFv s m) || (isInFv s n)
isInFv s (TVar name) = s == name
