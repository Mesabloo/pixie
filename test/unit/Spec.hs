import Logger
import Types
import qualified Parser.Main as Parser
import qualified ExprTypeChecker.Main as ExprTypeChecker
import qualified ProgramTypeChecker.Main as ProgramTypeChecker
import qualified Interpreter.Main as Interpreter
import qualified CodeGenerator.Main as CodeGen
import Options.Applicative

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper) (fullDesc)
    run props =
        runSpec "parser" (Parser.run props)
        *> runSpec "expression type-checker" (ExprTypeChecker.run props)
        *> runSpec "program type-checker" (ProgramTypeChecker.run props)
        *> runSpec "interpreter" (Interpreter.run props)
        *> runSpec "code generator" (CodeGen.run props)

    options = Options <$> switch (long "debug" <> short 'd' <> help "Whether to print debug information or not")