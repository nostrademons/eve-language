module Main(main) where
import System
import IO
import Maybe
import Control.Monad.Error

import Expr (exprType)
import Lexer
import Parser
import TypeCheck
import Error
import Repl

parseLine input = putStrLn output
  where
    Right output = (lexer "stdin" input >>= parseRepl >>= typeCheck >>= return . show . fromJust . exprType) 
                        `catchError` (return . show)

main = runRepl "Eve" parseLine
