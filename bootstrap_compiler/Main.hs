module Main(main) where
import System
import IO
import Control.Monad.Error

import Lexer
import Parser
import Error
import Repl

parseLine input = putStrLn output
  where
    Right output = (lexer "stdin" input >>= parseRepl >>= return . show) `catchError` (return . show)

main = runRepl "Eve" parseLine
