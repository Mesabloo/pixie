import Logger
import Types
import qualified Parser.Main as Parser
import qualified ExprTypeChecker.Main as ExprTypeChecker
import qualified ProgramTypeChecker.Main as ProgramTypeChecker

main :: IO ()
main =
    runSpec "parser" Parser.run
    *> runSpec "expression type-checker" ExprTypeChecker.run
    *> runSpec "program type-checker" ProgramTypeChecker.run