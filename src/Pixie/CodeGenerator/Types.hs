{-# LANGUAGE ExistentialQuantification, LambdaCase #-}

module Pixie.CodeGenerator.Types where

import Data.Char
import Data.Text hiding (toLower, concatMap)

-- https://c9x.me/x86/
-- http://www.eecg.toronto.edu/~amza/www.mindsec.com/files/x86regs.html

data Target = NASM | GAS
    deriving (Eq, Show)

-----------------------------------------------------------------------------------

class Convertible a where
    toString :: a -> Target -> String