{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixie.Parser.Types where

newtype Program = Program [Statement]

data Statement
    = Function { funName :: Name, body :: Body, retType :: Type, args :: [Var] }
    | Global { globalName :: Name, value :: Expr }
-- For now

type Body = [FunStatement]

data Type
    = Int | Float | Char

data Var = Var { valName :: Name, valType :: Type }

type Name = String

data FunStatement
    = VarDef Name Type Expr
    | Expression Expr
    | Return Expr

data Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lit LitExpr

data LitExpr
    = LInt Integer
    | LFloat Double
    | LChar Char