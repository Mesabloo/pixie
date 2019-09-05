{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixie.Interpreter.Types where

import Control.Monad.Reader
import Control.Monad.Except
import Text.PrettyPrint.Leijen
import qualified Data.Map as Map
import Data.Text
import Pixie.Parser.Types

type Eval = ReaderT EvalEnv (Except EvalError) 

type EvalError = Doc

newtype EvalEnv = EvalEnv (Map.Map Text Value)
    deriving (Show, Monoid, Semigroup)

data Value
    = VInt Int
    | VFloat Double
    | VChar Char
    | VVoid
    | VFun [Text] Body
    deriving Show

wrapEvalEnv :: Map.Map Text Value -> EvalEnv
wrapEvalEnv = EvalEnv

unwrapEvalEnv :: EvalEnv -> Map.Map Text Value
unwrapEvalEnv (EvalEnv m) = m