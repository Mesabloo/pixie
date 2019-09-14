{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module CodeGenerator.Main where

import Logger
import Types
import Data.Key
import qualified Pixie.Parser.Types as P
import qualified Pixie.CodeGenerator.Types as C
import qualified Pixie.CodeGenerator.Module.Types as C
import qualified Pixie.CodeGenerator.AST.Types as C
import qualified Pixie.CodeGenerator.Module.Converter as Conv
import Pixie.CodeGenerator.Generator
import Control.Monad.State

run :: Options -> Spec ()
run Options{..} = sequence_ $ mapWithKey testCodeGenerator testPrograms
  where
    testCodeGenerator :: Int -> (P.Program, C.Module, Bool) -> Spec ()
    testCodeGenerator index (parsed, expected, shouldFail) =
        let module' = generateFromProgram parsed
            stringified = Conv.moduleToString module' C.GAS
            indexShow = putStr ("Test " <> show (index + 1) <> " of " <> show (length testPrograms) <> ": ")
        in liftIO $ indexShow *> putStrLn ("\n" <> stringified)

testPrograms :: [(P.Program, C.Module, Bool)]
testPrograms = [ (P.Program [ P.Function "main" P.Int [] [ P.Return (P.Lit $ P.LInt 0) ] ], mempty, True) ]