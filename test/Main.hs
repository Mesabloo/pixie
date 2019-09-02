{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}

module Main where

import Data.Text
import Text.Megaparsec
import Pixie.Parser.Parser
import System.Exit
import Text.RawString.QQ

main :: IO ()
main = putStrLn "" *> mapM_ handle testStrings
  where
    handle :: Text -> IO ()
    handle text =
        let parsed = parse pProgram "tests" text
        in pure parsed >>= \case
            Left err -> putStrLn (errorBundlePretty err) >> exitFailure
            Right x -> print x

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