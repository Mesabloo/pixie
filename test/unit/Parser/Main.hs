{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}

module Parser.Main where

import Data.Text hiding (length)
import Text.Megaparsec
import Pixie.Parser.Parser
import System.Exit
import Text.RawString.QQ
import Data.Key
import Logger
import Control.Monad.State
import Types

run :: Spec ()
run = sequence_ (mapWithKey handle testStrings)
  where
    handle :: Int -> Text -> Spec ()
    handle index text =
        let parsed = parse pProgram "tests" text
            indexShow = putStr ("Test " <> show (index + 1) <> " of " <> show (length testStrings) <> ": ")
        in parsed <$ liftIO indexShow >>= \case
            Left err -> liftIO ((bold . red) (putStrLn "Failed!") *> red (putStrLn (errorBundlePretty err))) *> put True
            Right res -> liftIO ((bold . green) (putStrLn "Passed!") *> blue (print res))

testStrings :: [Text]
testStrings = [ [r| fn main(): int {} |]
              , [r| fn test(x: int, y: int): int {} |]
              , [r| fn main(): int
                    {
                        let x: int = 0;
                        ret x;
                    } |]
              , [r| fn test2(x: int): int {
                        let y: int = (x + 1) * 2;
                        ret y;
                    } |]
              , [r| let x: float = -0.0; |]
              , [r| let y: void = puts('a'); |]]