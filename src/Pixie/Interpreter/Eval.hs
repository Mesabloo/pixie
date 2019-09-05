{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-}

module Pixie.Interpreter.Eval where

import Pixie.Interpreter.Types
import Pixie.Parser.Types
import Text.PrettyPrint.Leijen hiding ((<$>))
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Text hiding (zip)
import Debug.Trace

evalExpr :: Expr -> Eval Value
evalExpr (Lit l) =
    let evalLit = \case
            LInt i -> VInt i
            LFloat f -> VFloat f
            LChar c -> VChar c
    in pure $ evalLit l
evalExpr (VarId name) =
    lookupEnv name
evalExpr (FunCall name args) = do
    vfun <- lookupEnv name >>= \case
        v@(VFun _ _) -> pure v
        _ -> throwError (text "Incorrect value retrieved, should be a function!")
    let VFun argNames body = vfun
    vArgs <- forM (zip argNames args) $
        \(name, expr) -> (name,) <$> evalExpr expr
    let argsEnv = Map.fromList vArgs
    local (wrapEvalEnv . (argsEnv `Map.union`) . unwrapEvalEnv) (evalFunStatements body)
evalExpr e = throwError (text "Unimplemented")

evalFunStatements :: [FunStatement] -> Eval Value
evalFunStatements [] = pure VVoid
evalFunStatements (Return e:_) = let expr = evalExpr e in expr
evalFunStatements (Expression e:ss) = evalExpr e *> evalFunStatements ss
evalFunStatements (VarDef name _ e:ss) = do
    ev <- evalExpr e
    local (wrapEvalEnv . (Map.singleton name ev `Map.union`) . unwrapEvalEnv) (evalFunStatements ss)

evalProgram :: Program -> Eval ()
evalProgram (Program []) = pure ()
evalProgram (Program (Global name _ val:ss)) = do
    e <- evalExpr val
    local (wrapEvalEnv . (Map.singleton name e `Map.union`) . unwrapEvalEnv) (evalProgram (Program ss))
evalProgram (Program (Function name _ args body:ss)) =
    local (wrapEvalEnv . (Map.singleton name (VFun (varName <$> args) body) `Map.union`) . unwrapEvalEnv) (evalProgram (Program ss))

lookupEnv :: Text -> Eval Value
lookupEnv name =
    asks (Map.lookup name . unwrapEvalEnv) >>= \case
        Just v -> pure v
        Nothing -> throwError (text $ "`" <> unpack name <> "` not found in evaluation environment.")

-----------------------------------------------------------------------------------------------------------------

runEval :: Eval a -> Either EvalError a
runEval e =
    let r = mempty
    in runExcept $ runReaderT e r