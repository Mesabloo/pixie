module Pixie.CodeGenerator.Module.Converter where

import Pixie.CodeGenerator.Module.Types
import Pixie.CodeGenerator.AST.Types
import Pixie.CodeGenerator.Types

moduleToProgram :: Module -> Program
moduleToProgram (Module funs globs) =
    let convFuns  = concatMap (\(Function name _ is) -> [LABEL name, PUSH EBP, MOV EBP ESP] ++ is ++ [POP EBP, RET]) funs
        convGlobs = concatMap (\(Global name _) -> [LABEL name]) globs
    in  Program (convGlobs <> convFuns)

moduleToString :: Module -> Target -> String
moduleToString mod t = moduleToProgram mod `toString` t