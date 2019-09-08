module MephistoParser
  (
    readExpr
  ) where

import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import MephistoTypes
import Control.Monad
import Control.Monad.Except
import Data.List
import Control.Monad.State as S
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec.Token as T

type StateContext = S.State (M.Map String Int, [String])

type MephistoParser m = ParsecT String () StateContext m

baseTypes = [("Bool", MBool)]

mephisto = T.LanguageDef {
  T.commentStart = "{-",
  T.commentEnd = "-}",
  T.commentLine = "--",
  T.nestedComments = True,
  T.identStart = letter :: (MephistoParser Char),
  T.identLetter = alphaNum :: (MephistoParser Char),
  T.reservedNames = ["if", "then", "else", "->"],
  T.reservedOpNames = [],
  T.caseSensitive = True
                    }

lexer = T.makeTokenParser mephisto

ident :: MephistoParser String
ident = T.identifier lexer

reserved :: String -> MephistoParser ()
reserved = T.reserved lexer

parens :: MephistoParser Term -> MephistoParser Term
parens = T.parens lexer

wspace :: MephistoParser ()
wspace = T.whiteSpace lexer

brackets :: MephistoParser Term -> MephistoParser Term
brackets = T.brackets lexer

fBrackets :: MephistoParser Type -> MephistoParser Type
fBrackets = T.brackets lexer

braces :: MephistoParser Type -> MephistoParser Type
braces = T.braces lexer

dot :: MephistoParser String
dot = T.dot lexer

comma :: MephistoParser String
comma = T.comma lexer

colon :: MephistoParser String
colon = T.colon lexer

angles :: MephistoParser Type -> MephistoParser Type
angles = T.angles lexer

pAtomic :: MephistoParser Type
pAtomic = do
  name <- ident
  case (lookup name baseTypes) of
    Just x -> return $ Base x
    Nothing -> return $ TVar name

pFunc :: MephistoParser Type
pFunc = fBrackets $ do
  from <- pType
  reserved "->"
  to <- pType
  return $ Func from to

pTuple :: MephistoParser Type
pTuple = angles $ do
  types <- sepBy1 pType comma
  return $ MTuple types

pSum :: MephistoParser Type
pSum = braces $ do
  a <- pType
  comma
  b <- pType
  return $ Sum a b 

pType :: MephistoParser Type
pType = pAtomic <|> pFunc <|> pTuple <|> pSum

pVar :: MephistoParser Term
pVar = do
  name <- ident
  (mappings, boundV) <- lift S.get 
  case (elemIndex name boundV) of
    Just x -> return $ Var x
    Nothing -> do
      case (M.lookup name mappings) of
        Just y -> return $ Var (y + (length boundV))
        Nothing -> do
          lift $ S.put (M.insert name (M.size mappings) mappings, boundV)
          return $ Var ((M.size mappings) + (length boundV))

pIf :: MephistoParser Term
pIf = do
  reserved "if" 
  cond <- pExpr
  reserved "then"
  thenForm <- pExpr
  reserved "else"
  elseForm <- pExpr
  return $ If cond thenForm elseForm

pApp :: MephistoParser Term
pApp = brackets $ do
  func <- pExpr
  arg <- pExpr
  return $ App func arg

pAbs :: MephistoParser Term
pAbs = parens $ do
  name <- ident
  colon 
  typ <- pType
  dot
  (mappings, boundV) <- lift S.get
  lift $ S.put (mappings, [name]++boundV)
  term <- pExpr
  return $ Abs typ name term
  
pExpr = pAbs <|> pApp <|> pIf <|> pVar 

readExpr :: String -> StateContext (ThrowsError Term)
readExpr input =   flip fmap (runParserT pExpr () "" input) 
                   (\x -> case x of
                       Left err -> throwError $ Parser err
                       Right term -> return term)
