{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixie.Parser.Types where

newtype Program = Program [Statement]
    deriving Show

data Statement
    = Function { funName :: Name, body :: Body, retType :: Type, args :: [Var] }
    | Global { globalName :: Name, value :: Expr }
    deriving Show
-- For now

type Body = [FunStatement]

data Type
    = Int | Float | Char
    deriving Show

data Var = Var { valName :: Name, valType :: Type }
    deriving Show

type Name = String

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
    | FunCall Name [Expr]
    deriving Show

data LitExpr
    = LInt Integer
    | LFloat Double
    | LChar Char
    deriving Show