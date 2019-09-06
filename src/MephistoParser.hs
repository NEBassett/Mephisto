module MephistoParser
  (
    readExpr
  ) where

import Text.Parsec
import Text.ParserCombinators.Parsec
import MephistoTypes
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec.Token as T

type StateContext = State (M.Map String Int, [String])

type MephistoParser m = ParsecT String () StateContext m

mephisto = emptyDef {
  T.commentStart = "{-",
  T.commentEnd = "-}",
  T.commentLine = "--",
  T.identStart = letter,
  T.identLetter = alphaNum,
  T.reservedNames = ["if", "then", "else"],
  T.reservedOpNames = []
                    }

lexer = T.makeTokenParser mephisto
ident = T.identifier lexer
res = T.reserved lexer
parens = T.parens lexer
wspace = T.whiteSpace lexer
brackets = T.brackets lexer

pVar :: MephistoParser Term
pVar = do
  name <- ident
  (mappings, boundV) <- lift get 
  case (elemIndex name boundV) of
    Just x -> return $ Var x
    Nothing -> do
      case (M.lookup name mappings) of
        Just y -> return $ Var (y + (length boundV))
        Nothing -> do
          lift $ put (M.insert name (M.size mappings)

pIf :: MephistoParser Term
pIf = do
  try $ reserved "if" 
  cond <- pExpr
  try $ reserved "then"
  thenForm <- pExpr
  try $ reserved "else"
  elseForm <- pExpr
  return $ If cond thenForm elseForm



pAbs :: MephistoParser Term
pAbs = parens $ do
  name <- ident
  T.colon
  typ <- pType
  T.dot
  (mappings, boundV) <- lift get
  lift $ put (mappings, [name]++boundV)
  term <- pExpr
  return $ Abs typ name term
  
  
  
pExpr = pAbs <|> pApp <|> pIf <|> pVar 

readExpr :: String -> ThrowsError Term
readExpr input = case  (parse pExpr "" input) of
                   Left err -> throwError $ Parser err
                   Right term -> return term
