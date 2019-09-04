{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pixie.TypeChecker.Types where

import Control.Monad.RWS
import Control.Monad.State
import Data.Text
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen
import Data.Void

data Type = TInt | TFloat | TChar | TVoid
          | TFun Type [Type]
    deriving (Eq, Ord, Show)

type TIError = Doc

newtype TypeEnv = TypeEnv (Map.Map String Type)
    deriving (Show, Monoid, Semigroup)

type Check = RWS TypeEnv [TIError] Int