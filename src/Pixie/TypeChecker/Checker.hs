{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Pixie.TypeChecker.Checker where

import Pixie.TypeChecker.Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.Text as Text
import qualified Pixie.Parser.Types as P (Type(..))
import Pixie.Parser.Types hiding (Type(..))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen
import Control.Monad.Writer

tcExpr :: Expr -> Check Type
tcExpr (Lit l) =
    let tcLit = \case
            LInt _ -> TInt
            LFloat _ -> TFloat
            LChar _ -> TChar
    in pure $ tcLit l
tcExpr e = censor (text "Type-checking for expression “" <> text (show e) <> text "” is not yet implemented." :) (pure TVoid)

-------------------------------------------------------------------------------------------------------

runCheck :: Check a -> (a, [TIError])
runCheck c =
    let r = mempty
        s = 0
    in evalRWS c r s