{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}

module Pixie.TypeChecker.Checker where

import Pixie.TypeChecker.Types
import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Text as Text
import qualified Pixie.Parser.Types as P (Type(..))
import Pixie.Parser.Types hiding (Type(..))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen hiding ((<$>))
import Control.Monad.Writer
import Control.Applicative
import Control.Monad.Reader
import Pixie.TypeChecker.Errors

tcExpr :: Expr -> Check Type
tcExpr (Lit l) =
    let tcLit = \case
            LInt _ -> TInt
            LFloat _ -> TFloat
            LChar _ -> TChar
    in pure $ tcLit l
tcExpr (VarId name) =
    lookupEnv name
tcExpr (FunCall name args) =
    lookupEnv name >>= \case
        TFun argsT retT ->
            let checkArgs = do
                    res <- mapM tcExpr args
                    if length argsT == length res
                    then forM_ (zip argsT res) $ uncurry unify
                    else censor (makeWrongNumberOfArgsError (Text.unpack name) (length argsT) (length res) :) (pure ())
            in retT <$ checkArgs
        _ -> censor (makeNotAFunctionError (Text.unpack name) :) (pure TVoid)
tcExpr (Add e1 e2) = numOpCheck e1 e2 False
tcExpr (Sub e1 e2) = numOpCheck e1 e2 False
tcExpr (Mul e1 e2) = numOpCheck e1 e2 False
tcExpr (Div e1 e2) = numOpCheck e1 e2 True
tcExpr e = censor (text "Type-checking for expression `" <> text (show e) <> text "` is not yet implemented." :) (pure TVoid)

numOpCheck :: Expr -> Expr -> Bool -> Check Type
numOpCheck e1 e2 isDiv = do
    et1 <- tcExpr e1
    et2 <- tcExpr e2
    () <$ unify et1 et2
    
    if isDiv
    then () <$ unify et1 TFloat
    else pure ()

    if isNumType et1 && isNumType et2
    then pure et1
    else censor (makeInvalidNumTypeError (if isNumType et1 then et2 else et1) :) (pure TVoid)

unify :: Type -> Type -> Check Type
unify t1 t2 | t1 /= t2 = censor (makeUnifyError t1 t2 :) (pure TVoid)
            | otherwise = pure t1

lookupEnv :: Text.Text -> Check Type
lookupEnv name =
    asks (Map.lookup name . unwrapTypeEnv) >>= \case
        Nothing -> censor (makeUnboundIdError (Text.unpack name) :) (pure TVoid)
        Just t -> pure t

tcType :: P.Type -> Type
tcType P.Int = TInt
tcType P.Float = TFloat
tcType P.Char = TChar
tcType P.Void = TVoid

-------------------------------------------------------------------------------------------------------

tcProgram :: Program -> Check ()
tcProgram (Program []) = pure ()
tcProgram (Program (Function{..}:ss)) =
    let t = tcType retType
        args' = liftA2 (,) varName (tcType . varType) <$> args
    in do
        ret <- local (wrapTypeEnv . (Map.fromList args' `Map.union`) . unwrapTypeEnv) (tcBody body)
        () <$ unify t ret
        let funVal = TFun (snd <$> args') t
        local (wrapTypeEnv . (Map.singleton funName funVal `Map.union`) . unwrapTypeEnv) (tcProgram (Program ss))
tcProgram (Program (Global{..}:ss)) = do
    te <- tcExpr globalValue
    () <$ unify (tcType globalType) te
    local (wrapTypeEnv . (Map.singleton globalName te `Map.union`) . unwrapTypeEnv) (tcProgram (Program ss))

tcBody :: Body -> Check Type
tcBody [] = do
    st <- get <* put []

    if null st
    then pure TVoid
    else
        let first = head st
        in first <$ sequence_ (unify first <$> tail st)
tcBody (Return e:ss) = do
    expr <- tcExpr e
    modify (expr :)
    tcBody ss
tcBody (Expression e:ss) = tcExpr e *> tcBody ss
tcBody (VarDef name type' e:ss) = do
    exprT <- tcExpr e
    let t = tcType type'
    () <$ unify t exprT
    local (wrapTypeEnv . (Map.singleton name t `Map.union`) . unwrapTypeEnv) (tcBody ss)

-------------------------------------------------------------------------------------------------------

runCheck :: Check a -> (a, [TIError])
runCheck c =
    let r = mempty
        s = []
    in evalRWS c r s