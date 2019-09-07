{-# LANGUAGE KindSignatures, TypeFamilies, GADTs, RankNTypes #-}

module Pixie.CodeGenerator.Types where

import Data.Char
import Data.Word

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

data Value = Reg Register | Lit Literal

data Literal

data Instruction where
    MOV :: (Addressable a, Addressable b) => a -> b -> Instruction

newtype Label = Label { name :: String }

--------------------------------------------------------------------------------

class Convertible (a :: *) where
    convert :: Target -> a -> String

data Offsetted = forall a. Addressable a => a :# Int

class Addressable a where
    withOffset :: Int -> a -> Offsetted
    withOffset = flip (:#)

--------------------------------------------------------------------------------

instance Convertible Register where
    convert t r =
        let prefix = if t == GAS then "%" else ""
        in prefix <> (toLower <$> show r)