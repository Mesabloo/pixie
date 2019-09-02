module Types where

import Control.Monad.State
import Logger
import System.Exit

type Spec = StateT Bool IO

runSpec :: String -> Spec a -> IO a
runSpec name s = do
    putStrLn ""
    (underline . white) (putStrLn ("Testing “" <> name <> "”:"))

    (x, failed) <- runStateT s False
    x <$ if failed
         then (underline . red) (putStrLn ("Test suite “" <> name <> "” failed!")) *> exitFailure
         else (underline . green) (putStrLn ("Test suite “" <> name <> "” passed!"))
