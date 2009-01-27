module Main(main) where
import System
import IO
import Maybe
import Control.Monad.Error

import Expr (FileLineValue(..), exprType, fileLineVal)
import Lexer
import Parser
import TypeCheck
import Error
import Repl

parseLine input = putStrLn output
  where
    Right output = (lexer "stdin" input >>= parseRepl >>=  printReplLine . fileLineVal)
                        `catchError` (return . show)

printReplLine (NakedExpr expr) = liftM show $ typeCheck expr
printReplLine other = return $ show other

main = runRepl "Eve" parseLine
