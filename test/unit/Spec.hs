import Logger
import Types
import qualified Parser.Main as Parser
import qualified ExprTypeChecker.Main as ExprTypeChecker

main :: IO ()
main =
    runSpec "parser" Parser.run
    *> runSpec "type-checker" ExprTypeChecker.run