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
import Prelude hiding (div)

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
    vArgs <- forM (zip argNames args) $ \(name, expr) -> (name,) <$> evalExpr expr
    let argsEnv = Map.fromList vArgs
    local (wrapEvalEnv . (argsEnv `Map.union`) . unwrapEvalEnv) (evalFunStatements body)
evalExpr (Add e1 e2) =
    evalOperator add e1 e2
evalExpr (Sub e1 e2) =
    evalOperator sub e1 e2
evalExpr (Mul e1 e2) =
    evalOperator mul e1 e2
evalExpr (Div e1 e2) =
    evalOperator div e1 e2
evalExpr e = throwError (text "Unimplemented")

evalOperator :: (Value -> Value -> Eval Value) -> Expr -> Expr -> Eval Value
evalOperator f e1 e2 = do
    ev1 <- evalExpr e1
    ev2 <- evalExpr e2
    checkNumVal ev1 *> checkNumVal ev2
    f ev1 ev2

checkNumVal :: Value -> Eval ()
checkNumVal (VInt _) = pure ()
checkNumVal (VFloat _) = pure ()
checkNumVal v = throwError (text ("`" <> show v <> "` is not a numeric value!"))

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

-----------------------------------------------------------------------------------------------------------------

add :: Value -> Value -> Eval Value
add (VInt i1) (VInt i2) = pure $ VInt (i1 + i2)
add (VFloat f1) (VFloat f2) = pure $ VFloat (f1 + f2)
add _ _ = throwError (text "Type checking failed for the `+` operator.")

sub :: Value -> Value -> Eval Value
sub (VInt i1) (VInt i2) = pure $ VInt (i1 - i2)
sub (VFloat f1) (VFloat f2) = pure $ VFloat (f1 - f2)
sub _ _ = throwError (text "Type checking failed for the `-` operator.")

div :: Value -> Value -> Eval Value
div (VFloat f1) (VFloat f2) = pure $ VFloat (f1 / f2)
div _ _ = throwError (text "Type checking failed for the `/` operator.")

mul :: Value -> Value -> Eval Value
mul (VInt i1) (VInt i2) = pure $ VInt (i1 * i2)
mul (VFloat f1) (VFloat f2) = pure $ VFloat (f1 * f2)
mul _ _ = throwError (text "Type checking failed for the `*` operator.")