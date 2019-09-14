{-# LANGUAGE ExistentialQuantification #-}

module Pixie.CodeGenerator.AST.Types where

import Pixie.CodeGenerator.Types
import Data.Char

newtype Program
    = Program [Instruction]
    deriving Show

data Function
    = Function { funName :: String, funArgs :: [String], funBody :: [Instruction] }
    deriving Show

data Global
    = forall a. Valuable a => Global { globName :: String, globVal :: a }

data Value
    = Reg Register
    | ID AddressID
    | Number Int
    deriving Show

type AddressID = String

data Addressed a
    = a :# Int
    deriving Show

data Register
    -- general purpose registers
    = EAX     | EBX     | ECX     | EDX        -- ? 32 bits
    |    AX   |    BX   |    CX   |    DX      -- ? 16 bits
    | AH | AL | BH | BL | CH | CL | DH | DL    -- ? 8 bits

    -- pointer specific registers
    | EDI | ESI | EBP | ESP | EIP {- UNUSED -} -- ? 32 bits
    | DI  | SI  | BP  | SP  | IP               -- ? 16 bits

    -- segments
    | CS | DS | ES | FS | GS | SS
    deriving (Show, Eq)

newtype Number
    = N Int

data Instruction
    = forall a b. (Addressable a, Valuable b) => MOV { movDest :: a, movSrc :: b }
    | forall a. Valuable a => PUSH { pushVal :: a }
    | forall a. Addressable a => POP { popVal :: a }
    | LABEL AddressID
    | RET

------------------------------------------------------------------------------------------------------

class Convertible a => Valuable a where
    toValue :: a -> Value

class Convertible a => Addressable a where
    toOffset :: a -> Int -> Addressed a
    toOffset = (:#)

-----------------------------------------------------------------------------------------------------

instance Show Instruction where
    show (MOV _ _) = "mov"
    show (PUSH _) = "push"
    show (POP _) = "pop"
    show (LABEL x) = x <> ":"
    show RET = "ret"

instance Show Global where
    show (Global x _) = x

instance Convertible Program where
    toString (Program is) t = concatMap ((<> "\n") . (`toString` t)) is

instance Convertible Instruction where
    toString i t = case i of
        RET -> "ret"
        LABEL l -> l <> ":"
        POP a -> "pop " <> toString a t
        PUSH a -> "push " <> toString (toValue a) t
        MOV dest src ->
            let suffix = if t == GAS
                         then toString src t <> ", " <> toString dest t
                         else toString dest t <> ", " <> toString src t
            in "mov " <> suffix
        _ ->
            if t == GAS
            then "/* Cannot stringify instruction " <> show i <> ". */"
            else ";; Cannot stringify instruction " <> show i <> "."

instance Convertible Number where
    toString (N n) t = if t == GAS then "$" <> show n else show n

instance Convertible Register where
    toString r t =
        let prefix = if t == GAS then "%" else ""
        in prefix <> (toLower <$> show r)

instance Convertible Value where
    toString (Reg r) t = toString r t
    toString (Number n) t = toString (N n) t
    toString (ID i) t = i

instance Addressable Register

instance Valuable Register where
    toValue = Reg

instance Valuable Number where
    toValue (N x) = Number x