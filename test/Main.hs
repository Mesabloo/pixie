{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Data.Text
import Text.Megaparsec
import Pixie.Parser.Parser
import System.Exit

main :: IO ()
main = mapM_ handle testStrings
  where
    handle :: Text -> IO ()
    handle text =
        let parsed = parse pProgram "tests" text
        in pure parsed >>= \case
            Left err -> putStrLn (errorBundlePretty err) >> exitFailure
            Right x -> print x

testStrings :: [Text]
testStrings = [""]