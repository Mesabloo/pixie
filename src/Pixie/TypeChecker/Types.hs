module Pixie.TypeChecker.Types where

import Control.Monad.RWS
import Data.Text
import qualified Data.Map as Map

data Type
    = TVoid | TInteger | TFloat | TChar
    deriving Show

data Constraint = Unify Type Type

type Check = RWS TypeEnv [Constraint] [Text]

data TypeEnv
    = TypeEnv { defined :: Map.Map Text Type }