{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module ProgramTypeChecker.Main where

import Types
import Logger
import Control.Monad.State
import Pixie.Parser.Types
import Data.Key
import Pixie.TypeChecker.Checker
import Control.Applicative

run :: Options -> Spec ()
run Options{..} =
    sequence_ (mapWithKey testTC testPrograms)
  where
    testTC :: Int -> (Program, Bool) -> Spec ()
    testTC index (p, shouldFail) =
        let res = runCheck (tcProgram p)
            indexShow = putStr ("Test " <> show (index + 1) <> " of " <> show (length testPrograms) <> ": ")
        in  liftIO indexShow
            *> case res of
                (x, []) -> liftIO $ (bold . green) (putStrLn "Passed!")
                (x, errs)
                    | not shouldFail -> 
                        liftIO ((bold . red) (putStrLn "Failed!")
                            *> red (mapM_ print errs))
                        *> put True
                    | otherwise ->
                        liftIO $ do
                            (bold . green) (putStrLn "Passed!")
                            guard (not debug)
                                <|> (blue (putStrLn "Type checking errored out as intended with errors:")
                                    *> blue (mapM_ ((*>) (putStr "> ") . print) errs))

testPrograms :: [(Program, Bool)]
testPrograms = [ (Program [Function "main" Int [] []], True)
               , (Program [Function "main" Int [] [VarDef "x" Int (Lit (LInt 0)), Return (VarId "x")]], False)
               , (Program [Global "x" Float $ Lit (LFloat (-0.0))], False)
               , (Program [Function "f" Void [] [], Global "x" Void $ FunCall "f" []], False)
               , (Program [Function "test2" Int [Var "x" Int] [VarDef "y" Int (Mul (Add (VarId "x") (Lit (LInt 1))) (Lit (LInt 2))), Return (VarId "y")]], False)
               , (Program [Function "main" Int [] [VarDef "x" Int (Lit (LInt 0)), Return (Lit $ LFloat 0)]], True) ]