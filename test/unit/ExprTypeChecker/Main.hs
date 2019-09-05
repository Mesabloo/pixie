{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module ExprTypeChecker.Main where

import Logger
import System.Exit
import Types
import Control.Monad.State
import Pixie.TypeChecker.Types
import Pixie.Parser.Types hiding (Type(..))
import Pixie.TypeChecker.Checker
import Data.Key
import qualified Data.Text as Text
import Control.Applicative

run :: Options -> Spec ()
run Options{..} =
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
                            *> red (putStrLn ("Cannot unify type `" <> show x <> "` with expected type `" <> show t <> "`")))
                        *> put True
                    | otherwise -> liftIO $ do
                        (bold . green) (putStrLn "Passed!")
                        guard (not debug)
                            <|> blue (putStr (show e) *> putStr " :: " *> print x)
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

testExpressions :: [(Expr, Type, Bool)]
testExpressions = [ (Lit (LInt 0), TInt, False)
                  , (VarId "hello", TVoid, True)
                  , (FunCall "x" [Lit (LInt 1)] `Add` Lit (LFloat $ -1.3), TVoid, True)
                  , (FunCall "y" [Lit (LInt 0), Lit (LFloat 0.0)], TVoid, True)
                  , (Lit (LInt 0) `Add` Lit (LInt 1), TInt, False)
                  , (Lit (LFloat 0.0) `Add` Lit (LChar 'a'), TVoid, True) ]