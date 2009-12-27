module Main(main) where
import IO
import System

main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: eve <file to compile>"
    else do
      inFile <- openFile (args !! 0) ReadMode
      text <- hGetContents inFile
      putStrLn text
