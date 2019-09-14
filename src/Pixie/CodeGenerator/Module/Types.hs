module Pixie.CodeGenerator.Module.Types where

import Pixie.CodeGenerator.AST.Types

data Module
    = Module [Function] [Global]
    deriving Show

-------------------------------------------------------------------------------------

instance Semigroup Module where
    (<>) = mappend

instance Monoid Module where
    mempty = Module [] []
    mappend (Module _1 _2) (Module _3 _4) = Module (_1 <> _3) (_2 <> _4)