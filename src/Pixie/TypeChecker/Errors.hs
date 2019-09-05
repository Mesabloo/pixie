module Pixie.TypeChecker.Errors where

import Text.PrettyPrint.Leijen
import Pixie.TypeChecker.Types

makeWrongNumberOfArgsError :: String -> Int -> Int -> Doc
makeWrongNumberOfArgsError name expected got =
    text ("Wrong number of argument provided to function `" <> name
        <> "`: expected " <> show expected <> ", got " <> show got <> ".")

makeUnboundIdError :: String -> Doc
makeUnboundIdError name = text ("Undefined variable or function `" <> name <> "`.")

makeNotAFunctionError :: String -> Doc
makeNotAFunctionError name = text ("`" <> name <> "` is not a function.")

makeUnifyError :: Type -> Type -> Doc
makeUnifyError t1 t2 = text ("Cannot match type `" <> show t1 <> "` with type `" <> show t2 <> "`.")

makeInvalidNumTypeError :: Type -> Doc
makeInvalidNumTypeError t = text ("Invalid numeric type `" <> show t <> "`")