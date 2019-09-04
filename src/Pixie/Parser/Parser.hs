{-# LANGUAGE OverloadedStrings #-}

module Pixie.Parser.Parser where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text
import Data.Char
import Control.Monad
import Pixie.Parser.Types
import Control.Monad.Combinators.Expr

type Parser = M.Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space C.space1 lineCmnt blockCmnt)

lineCmnt :: Parser ()
lineCmnt = void $ string "//" *> M.anySingle `M.manyTill` C.eol

blockCmnt :: Parser ()
blockCmnt = void $ string "/*" *> M.anySingle `M.manyTill` string "*/"

string :: Text -> Parser Text
string = lexeme . C.string

string' :: Text -> Parser Text
string' = C.string

parens :: Parser a -> Parser a
parens = M.between (string "(") (string ")")

brackets :: Parser a -> Parser a
brackets = M.between (string "{") (string "}")

--------------------------------------------------------------------------

pChar :: Parser Char
pChar = string' "'" *> M.anySingle <* string "'"

pInt :: Parser Int
pInt = lexeme (L.signed C.space L.decimal)

pFloat :: Parser Double
pFloat = lexeme (L.signed C.space L.float)

pIdentifier :: Parser Text
pIdentifier = lexeme (M.try p) >>= check . pack
    where p = (:) <$> C.letterChar <*> M.many (C.letterChar M.<|> C.digitChar M.<|> C.char '_')
          check x | x `elem` keywords = fail "Keyword used as an identifier"
                  | otherwise         = pure x

pProgram :: Parser Program
pProgram = Program <$> (C.space *> M.many pStatement) <* M.eof

pStatement :: Parser Statement
pStatement = M.many (string ";") *> (pFunction M.<|> pGlobalVar)

pFunction :: Parser Statement
pFunction = do
    string "fn"
    id' <- pIdentifier
    args <- parens pArgList
    string ":"
    ret <- pType
    Function id' ret args <$> pBody
  where
    pArgList :: Parser [Var]
    pArgList = let arg = Var <$> (pIdentifier <* string ":") <*> pType
               in arg `M.sepBy` string ","

pGlobalVar :: Parser Statement
pGlobalVar = do
    VarDef id' type' expr <- pVarDef
    pure (Global id' type' expr)

pBody :: Parser Body
pBody = brackets $ M.many pBlockStatement

pType :: Parser Type
pType = int M.<|> float M.<|> char M.<|> void' -- for now
  where
    int = Int <$ string "int"
    float = Float <$ string "float"
    char = Char <$ string "char"
    void' = Void <$ string "void"

pExpr :: Parser Expr
pExpr = 
    let terms = M.choice
            [ Lit . LFloat <$> M.try pFloat
            , Lit . LInt <$> pInt
            , Lit . LChar <$> pChar
            , let args = pExpr `M.sepBy` string ","
              in M.try $ FunCall <$> pIdentifier <*> parens args
            , VarId <$> pIdentifier
            , parens pExpr ]
        operators = 
            [ [InfixL (Mul <$ string "*"), InfixL (Div <$ string "/")]
            , [InfixL (Add <$ string "+"), InfixL (Sub <$ string "-")] ]
    in makeExprParser terms operators


pBlockStatement :: Parser FunStatement
pBlockStatement = pRet M.<|> pVarDef M.<|> (Expression <$> pExpr)

pRet :: Parser FunStatement
pRet = string "ret" *> (Return <$> pExpr) <* string ";"

pVarDef :: Parser FunStatement
pVarDef = do
    string "let"
    id' <- pIdentifier
    string ":"
    type' <- pType
    string "="
    (VarDef id' type' <$> pExpr) <* string ";"

-----------------------------------------------------------------------------------

keywords :: [Text]
keywords = [ "int", "char", "float", "void", "fn", "let", "ret" ]