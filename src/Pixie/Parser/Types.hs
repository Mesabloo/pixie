{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixie.Parser.Types where

import Data.Text

newtype Program = Program [Statement]
    deriving Show

data Statement
    = Function { funName :: Name, retType :: Type, args :: [Var], body :: Body }
    | Global { globalName :: Name, globalType :: Type, globalValue :: Expr }
    deriving Show
-- For now

type Body = [FunStatement]

data Type
    = Int | Float | Char | Void
    deriving Show

data Var = Var { varName :: Name, varType :: Type }
    deriving Show

type Name = Text

data FunStatement
    = VarDef Name Type Expr
    | Expression Expr
    | Return Expr
    deriving Show

data Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lit LitExpr
    | VarId Name
    | FunCall Name [Expr]
    deriving Show

data LitExpr
    = LInt Int
    | LFloat Double
    | LChar Char
    deriving Show