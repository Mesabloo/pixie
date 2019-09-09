{-# LANGUAGE KindSignatures, GADTs, RankNTypes, TypeOperators, AllowAmbiguousTypes
           , FlexibleContexts, LambdaCase, FlexibleInstances, UndecidableInstances, DataKinds #-}

module Pixie.CodeGenerator.Types where

import Data.Char
import Data.Word
import Constraint.Unions

-- https://c9x.me/x86/
-- http://www.eecg.toronto.edu/~amza/www.mindsec.com/files/x86regs.html

data Target = NASM | GAS
    deriving (Eq, Show)

data Register
    -- general purpose registers
    = EAX     | EBX     | ECX     | EDX        -- ? 32 bits
    |    AX   |    BX   |    CX   |    DX      -- ? 16 bits
    | AH | AL | BH | BL | CH | CL | DH | DL    -- ? 8 bits

    -- pointer specific registers
    | EDI | ESI | EBP | ESP | EIP {- UNUSED -} -- ? 32 bits
    | DI  | SI                                 -- ? 16 bits

    -- segments
    | CS | DS | ES | FS | GS | SS
    deriving Show

data Literal where
    Number :: Int -> Literal
    Var :: Value a => String -> a -> Literal

data Instruction where
    MOV :: (Addressable a, Addressable b .| Value b) => a -> b -> Instruction
    PUSH :: Value a => a -> Instruction
    POP :: Value a => a -> Instruction

newtype Program = Program [Instruction]

data Offsetted = forall a. Addressable a => a :# Int

--------------------------------------------------------------------------------

class Convertible (a :: *) where
    convert :: Target -> a -> String

class Value a => Addressable a

class Value a

--------------------------------------------------------------------------------

instance Convertible Register where
    convert t r =
        let prefix = if t == GAS then "%" else ""
        in prefix <> (toLower <$> show r)

instance Convertible Literal where
    convert t = \case
        Number n -> let prefix = if t == GAS then "$" else "" in prefix <> show n
        Var i _ -> let prefix = if t == GAS then "$" else "" in prefix <> i


instance Addressable a => Convertible a where
    convert = convert

instance Addressable a => Convertible Offsetted where
    convert t (a :# offset) = let suffix = convert t a in show offset <> "(" <> suffix <> ")"

instance Value Register

instance Value Literal

instance Addressable Register

instance Addressable Literal