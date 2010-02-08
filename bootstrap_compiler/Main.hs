module Main(main) where
import Data.Word
import IO
import System
import System.Cmd

import Lexer
import Parser
import TypeCheck
import CodeGen
import Expr
import Error

runtimeFiles = map ("../runtime/" ++) [
    "heap.c",
    "main.c",
    "process.c",
    "strings.c"
    ]

writeCompiledFile filename mod = do
  let bcFile = filename ++ ".bc"
  let sFile = filename ++ ".s"
  writeBitcode bcFile mod
  rawSystem "llc" [bcFile]
  rawSystem "gcc" ((sFile : runtimeFiles) ++ ["-g", "-o", filename])
  rawSystem "rm" [bcFile, sFile]
  return ()

compileFile filename = do
  inFile <- openFile (filename ++ ".eve") ReadMode
  text <- hGetContents inFile
  case compile filename text of
    Left error -> print error
    Right lines -> codegen lines >>= writeCompiledFile filename

compile :: String -> String -> Either EveError [FileLine]
compile filename text = do
  tokens <- lexer filename text
  parseTree <- parseFile tokens
  typeCheckFile parseTree
  return parseTree

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: eve <file to compile>"
    else compileFile (args !! 0)
