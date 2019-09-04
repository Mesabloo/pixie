{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module TypeChecker.Main where

import Logger
import System.Exit
import Types
import Control.Monad.State
import Pixie.TypeChecker.Types
import Pixie.Parser.Types hiding (Type(..))
import Pixie.TypeChecker.Checker
import Data.Key
import qualified Data.Text as Text

run :: Spec ()
run =
    sequence_ (mapWithKey testTC testExpressions)
  where
    testTC :: Int -> (Expr, Type, Bool) -> Spec ()
    testTC index (e, t, shouldFail) =
        let res = runCheck (tcExpr e)
            indexShow = putStr ("Test " <> show (index + 1) <> " of " <> show (length testExpressions) <> ": ")
        in  liftIO indexShow
            *> case res of
                (x, [])
                    | x /= t ->
                        liftIO ((bold . red) (putStrLn "Failed!")
                            *> red (putStrLn "Cannot unify expr type with expected type"))
                        *> put True
                    | otherwise -> liftIO $ do
                        (bold . green) (putStrLn "Passed!")
                        blue (putStr (show e) *> putStr " :: " *> print x)
                (x, errs)
                    | not shouldFail -> 
                        liftIO ((bold . red) (putStrLn "Failed!")
                            *> red (mapM_ print errs))
                        *> put True
                    | otherwise ->
                        liftIO $ (bold . green) (putStrLn "Passed!")
                            *> green (putStrLn "Type checking errored out as intended:"
                                *> mapM_ ((*>) (putStr "> ") . print) errs)

testExpressions :: [(Expr, Type, Bool)]
testExpressions = [ (Lit (LInt 0), TInt, False)
                  , (VarId "hello", TVoid, True)
                  , (FunCall "x" [Lit (LInt 1)] `Add` Lit (LFloat $ -1.3), TVoid, True) ]