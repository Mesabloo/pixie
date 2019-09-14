{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Pixie.CodeGenerator.Generator where

import Pixie.CodeGenerator.Types
import qualified Pixie.Parser.Types as P
import Data.Char
import Pixie.CodeGenerator.Module.Types
import Pixie.CodeGenerator.AST.Types
import Data.Text (unpack)

generateFromProgram :: P.Program -> Module
generateFromProgram (P.Program []) = mempty
generateFromProgram (P.Program (f@P.Function{}:ss)) =
    Module [generateFromFunction f] [] <> generateFromProgram (P.Program ss)

generateFromFunction :: P.Statement -> Function
generateFromFunction P.Function{..} = Function (unpack funName) (unpack . P.varName <$> args) (generateFromBody args body)
generateFromFunction s = error ("Could not generate function code from " <> show s)

generateFromBody :: [P.Var] -> P.Body -> [Instruction]
generateFromBody [] [] = []
generateFromBody [] (i:is) = generateFromInstruction i ++ generateFromBody [] is
generateFromBody _ _ = error "Not yet implemented"

generateFromInstruction :: P.FunStatement -> [Instruction]
generateFromInstruction (P.Return e) =
    let (reg, instructions) = generateFromExpr e
    in instructions ++ if reg /= EAX then [ MOV EAX reg ] else [ ]
generateFromInstruction _ = error "Not yet implemented"

generateFromExpr :: P.Expr -> (Register, [Instruction])
generateFromExpr (P.Lit l) =
    let generateFromLit = \case
            P.LInt i -> (EAX, [ MOV EAX (N i) ])
            P.LChar c -> (EAX, [ MOV EAX (N $ ord c) ])
            P.LFloat f -> undefined
    in generateFromLit l
generateFromExpr _ = error "Not yet implemented"