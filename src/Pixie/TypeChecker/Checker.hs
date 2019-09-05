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
tcExpr (Add e1 e2) = numOpCheck e1 e2
tcExpr (Sub e1 e2) = numOpCheck e1 e2
tcExpr (Mul e1 e2) = numOpCheck e1 e2
tcExpr (Div e1 e2) = numOpCheck e1 e2
tcExpr e = censor (text "Type-checking for expression `" <> text (show e) <> text "` is not yet implemented." :) (pure TVoid)

numOpCheck :: Expr -> Expr -> Check Type
numOpCheck e1 e2 = do
    et1 <- tcExpr e1
    et2 <- tcExpr e2
    () <$ unify et1 et2
    if isNumType et1 && isNumType et2
    then pure et1
    else censor (makeInvalidNumTypeError (if isNumType et1 then et2 else et1) :) (pure TVoid)

unify :: Type -> Type -> Check Type
unify t1 t2 | t1 /= t2 = censor (makeUnifyError t1 t2 :) (pure TVoid)
            | otherwise = pure t1

lookupEnv :: Text.Text -> Check Type
lookupEnv name =
    gets (Map.lookup name . unwrapTypeEnv) >>= \case
        Nothing -> censor (makeUnboundIdError (Text.unpack name) :) (pure TVoid)
        Just t -> pure t

tcType :: P.Type -> Type
tcType P.Int = TInt
tcType P.Float = TFloat
tcType P.Char = TChar
tcType P.Void = TVoid

-------------------------------------------------------------------------------------------------------

tcProgram :: Program -> Check ()
tcProgram (Program stts) = mapM_ tcStatement stts

tcStatement :: Statement -> Check ()
tcStatement Function{..} =
    let t = tcType retType
        args' = liftA2 (,) varName (tcType . varType) <$> args
    in do
        modify (wrapTypeEnv . (Map.singleton funName (TFun (snd <$> args') t) `Map.union` Map.fromList args' `Map.union`) . unwrapTypeEnv)
        ret <- tcBody body
        () <$ unify t ret
tcStatement Global{..} = do
    te <- tcExpr globalValue
    tRes <- unify (tcType globalType) te
    modify (wrapTypeEnv . (Map.singleton globalName tRes `Map.union`) . unwrapTypeEnv)

tcBody :: Body -> Check Type
tcBody stts = do
    instrs <- mapM tcBodyStatement stts
    pure $ if null instrs then TVoid else last instrs

tcBodyStatement :: FunStatement -> Check Type
tcBodyStatement (Return e) = tcExpr e
tcBodyStatement (VarDef name type' e) = do
    exprT <- tcExpr e
    let t = tcType type'
    () <$ unify t exprT
    TVoid <$ modify (wrapTypeEnv . (Map.singleton name t `Map.union`) . unwrapTypeEnv)
tcBodyStatement (Expression e) = TVoid <$ tcExpr e

-------------------------------------------------------------------------------------------------------

runCheck :: Check a -> (a, [TIError])
runCheck c =
    let r = 0
        s = mempty
    in evalRWS c r s