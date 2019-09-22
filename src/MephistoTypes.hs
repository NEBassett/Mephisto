module MephistoTypes
  (
    Term(..),
    Context(..),
    ThrowsError(..),
    MError(..),
    Type(..),
    Bind(..),
    Atomic(..),
    typeSubst,
    typeShift,
    outerTypeSubst,
    termMap,
    shift,
    subst,
    outerSubst,
    outerTypeTermSubst
  ) where



import qualified Data.Map as M
import qualified Control.Lens as L
import Data.List
import Control.Monad.Except
import Text.Parsec

data Atomic = MBool | MDouble | MString | MNat deriving (Eq)

data Type = Base Atomic
          | Func Type Type
          | MTuple [Type]
          | Sum Type Type
          | TVar Int
          | All String Type
          | Some String Type

data Bind = Bound | Variable Type | BoundTVar

type Context = [(String, Bind)] -- its nice to have this as a list

data Term = Var Int -- De Bruijn index
          | Abs Type String Term
          | App Term Term
          | If Term Term Term
          | TTrue
          | TFalse
          | TTuple [Term]
          | Pack Type Term Type
          | Unpack String String Term Term
          | TAbs String Term
          | TApp Term Type 
          

data MError = NoRule Term
            | BadPredicate Term
            | NoType
            | BadType Type Type
            | BadTypeMsg Type String
            | Parser ParseError

type ThrowsError = Either MError

instance Show Term where show = (crawlingShow [])
instance Show Type where show = (showt [])
instance Show MError where show = showe
instance Show Atomic where show = showa
instance Eq Type where (==) = eq

eq :: Type -> Type -> Bool
eq (Base a) (Base b) = a == b
eq (Func a b) (Func c d) = (eq a c) && (eq b d)
eq (MTuple a) (MTuple b) = fst $ foldr (\m (n, (l:ls)) -> (((eq m l) && n), ls)) (True, b) a
eq (Sum a b) (Sum c d) = (eq a c) && (eq b d)
eq (TVar x) (TVar y) = x == y
eq (All s t) (All q r) = (s == q) && (t == r)
eq (Some s t) (Some q r) = (s == q) && (t == r)
eq _ _ = False

crawlingShow :: Context -> Term -> String
crawlingShow env (Abs typ hint term) =
  let (name, val) = getName env (hint, Variable typ)
      newEnv = [(name, val)] ++ env
  in "(lambda " ++  name ++ ". " ++ (crawlingShow newEnv term) ++ ")"

crawlingShow env (Var ind) = maybe ("Var: " ++ show ind) (\a -> a) (fmap fst (env  L.^? (L.element ind)))

crawlingShow env (App function argument) = "(" ++ (crawlingShow env function) ++ " " ++ (crawlingShow env argument) ++ ")"
crawlingShow env TTrue = "true"
crawlingShow env TFalse = "false"
crawlingShow env (TTuple ms) = "(" ++ (intercalate ", " (fmap (crawlingShow env) ms)) ++ ")" 
crawlingShow env (If a b c) = "If " ++ (crawlingShow env a) ++ " then " ++ (crawlingShow env b) ++ " else " ++ (crawlingShow env c)
crawlingShow env (Pack ty0 term ty1) = "{*" ++ (show ty0) ++ ", " ++ (crawlingShow env term) ++ "} as " ++ (show ty1)
crawlingShow env (Unpack tyName termName term0 term1) = "open " ++ (show term0) ++ " as {" ++ tyName ++ ", " ++ termName ++ "} in " ++ (show term1)
crawlingShow env (TAbs hint term) =
  let (name, val) = getName env (hint, BoundTVar)
      newEnv = [(name, val)] ++ env
  in "(lambda " ++  name ++ ". " ++ (crawlingShow newEnv term) ++ ")"
crawlingShow env (TApp func arg) = "[" ++ (crawlingShow env func) ++ " " ++ (show arg) ++ "]"

showa :: Atomic -> String
showa MBool = "Bool"
showa MNat = "Natural"
showa MDouble = "Double"
showa MString = "String" 

showt :: Context -> Type -> String
showt env (Base a) = showa a
showt env (Func a b) = "(" ++ (showt env a) ++ " -> " ++ (showt env b) ++ ")"
showt env (MTuple ms) = "(" ++ (intercalate ", " (fmap (showt env) ms)) ++ ")"
showt env (Sum a b) = (showt env a) ++ " + " ++ (showt env b)
showt env (TVar ind) = maybe ("Type Var: " ++ show ind) (\a -> a) (fmap fst (env  L.^? (L.element ind)))
showt env (All s t) =
  let (name, val) = getName env (s, BoundTVar)
      newEnv = [(name, val)] ++ env
  in "(∀" ++  name ++ ". " ++ (showt newEnv t) ++ ")"
showt env (Some s t) =
  let (name, val) = getName env (s, BoundTVar)
      newEnv = [(name, val)] ++ env
  in "{∃" ++  name ++ ", " ++ (showt newEnv t) ++ "}"

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

termMap :: (Int -> Term -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onTy c term = recurse c term
  where recurse :: Int -> Term -> Term
        recurse n val@(Var m) = onVar n val
        recurse n (Abs ty name aTerm) = Abs (onTy n ty) name (recurse (n+1) aTerm)
        recurse n (App term0 term1) = App (recurse n term0) (recurse n term1)
        recurse n (If term0 term1 term2) = If (recurse n term0) (recurse n term1) (recurse n term2)
        recurse n (TTuple terms) = TTuple (fmap (recurse n) terms)
        recurse n (Pack ty0 aTerm ty1) = Pack (onTy n ty0) (recurse n aTerm) (onTy n ty1)
        recurse n (Unpack name0 name1 term0 term1) = Unpack name0 name1 (recurse n term0) (recurse (n+2) term1)
        recurse n (TAbs name aTerm) = TAbs name (recurse (n+1) aTerm)
        recurse n (TApp aTerm ty) = TApp (recurse n aTerm) (onTy n ty) 
        recurse n val = val
        

shiftAbove :: Int -> Int -> Term -> Term
shiftAbove d z term = termMap (\c (Var m) -> if (m<c) then (Var $ m+d) else (Var m)) (typeShift d) z term

shift d term = shiftAbove d 0 term

subst d sub term = termMap (\c (Var m) -> if (m==c) then (shift c sub) else (Var m)) (const id) d term

typeTermSubst typ j term = termMap (const id) (\c t -> typeSubst c typ t) j term

outerSubst sub term = shift (-1) (subst 0 (shift 1 sub) term) 

outerTypeTermSubst typ term = shift (-1) (typeTermSubst (typeShift 1 0 typ) 0 term) 

typeShift :: Int -> Int -> Type -> Type
typeShift d c typ = recurse c typ
  where
    recurse n t = case t of
                    TVar m -> if (m<n) then t else TVar $ (m+d)
                    All s ty -> All s (recurse (n+1) ty)
                    Some s ty -> Some s (recurse (n+1) ty)
                    Sum ty0 ty1 -> Sum (recurse n ty0) (recurse n ty1)
                    MTuple ms -> MTuple (fmap (recurse n) ms)
                    Func ty0 ty1 -> Func (recurse n ty0) (recurse n ty1)
                    ty -> ty

typeSubst :: Int -> Type -> Type -> Type
typeSubst var sub typ = recurse 0 typ
  where
    recurse n t = case t of
                    TVar m -> if (m == (n+var)) then (typeShift n 0 sub) else t
                    All s ty -> All s (recurse (n+1) ty)
                    Some s ty -> Some s (recurse (n+1) ty)
                    Sum ty0 ty1 -> Sum (recurse n ty0) (recurse n ty1)
                    MTuple ms -> MTuple (fmap (recurse n) ms)
                    Func ty0 ty1 -> Func (recurse n ty0) (recurse n ty1)
                    ty -> ty

outerTypeSubst :: Type -> Type -> Type
outerTypeSubst sub typ = typeShift (-1) 0 (typeSubst 0 (typeShift 1 0 sub) typ)
