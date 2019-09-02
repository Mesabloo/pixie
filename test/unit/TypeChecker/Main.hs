module TypeChecker.Main where

import Logger
import System.Exit
import Types
import Control.Monad.State

run :: Spec ()
run = liftIO (red (putStrLn "Nothing to type-check for now.")) <* put True