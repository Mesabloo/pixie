{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Pixie.TypeChecker.Checker where

import Pixie.TypeChecker.Types
import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Text as Text
import qualified Pixie.Parser.Types as P (Type(..))
import Pixie.Parser.Types hiding (Type(..))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen
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
    lookupEnv name >>= pure
tcExpr (FunCall name args) =
    lookupEnv name >>= \case
        TFun argsT retT ->
            let checkArgs = do
                    res <- mapM tcExpr args
                    if length argsT == length res
                    then forM_ (zip argsT res) $
                        \(t1, t2) ->
                            if t1 /= t2
                            then censor (makeUnifyError t1 t2 :) (pure TVoid)
                            else pure TVoid
                    else censor (makeWrongNumberOfArgsError (Text.unpack name) (length argsT) (length res) :) (pure ())
            in retT <$ checkArgs
        _ -> censor (makeNotAFunctionError (Text.unpack name) :) (pure TVoid)
tcExpr e = censor (text "Type-checking for expression `" <> text (show e) <> text "` is not yet implemented." :) (pure TVoid)

lookupEnv :: Text.Text -> Check Type
lookupEnv name =
    asks (Map.lookup name . unwrapTypeEnv) >>= \case
        Nothing -> censor (makeUnboundIdError (Text.unpack name) :) (pure TVoid)
        Just t -> pure t

-------------------------------------------------------------------------------------------------------

runCheck :: Check a -> (a, [TIError])
runCheck c =
    let r = mempty
        s = 0
    in evalRWS c r s