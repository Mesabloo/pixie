{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Interpreter.Main where

import Pixie.Interpreter.Types
import Pixie.Interpreter.Eval
import Types
import Pixie.Parser.Types
import Data.Key
import Control.Monad.State
import Logger

run :: Options -> Spec ()
run Options{..} =
    sequence_ (mapWithKey evaluate testPrograms)
  where
    evaluate :: Int -> Program -> Spec ()
    evaluate index p = 
        let res = runEval (evalProgram p)
            indexShow = putStr ("Test " <> show (index + 1) <> " of " <> show (length testPrograms) <> ": ")
        in liftIO indexShow *> case res of
            Left err -> put True *> liftIO ((red . bold) (putStrLn "Failed!") *> red (print err))
            Right result -> liftIO ((green . bold) (putStrLn "Passed!"))

testPrograms :: [Program]
testPrograms = [ Program [Function "id" Int [Var "x" Int] [Return (VarId "x")]
                         , Global "x" Int (FunCall "id" [Lit (LInt 0)])]
               , Program [Function "id" Int [Var "x" Int] [Return (VarId "x" `Add` Lit (LInt 0))]
                         , Global "x" Int (FunCall "id" [Lit (LInt 0)])] ]