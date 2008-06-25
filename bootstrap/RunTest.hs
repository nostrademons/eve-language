module Main(main) where
import IO
import Monad
import Control.Monad.Trans
import Control.Monad.Error
import Directory
import Data.List

import Data
import Lexer
import Parser
import Eval
import Primitives

-- For some reason, isInfixOf isn't in my version of GHC.
contains [] needle = False
contains haystack@(x:xs) needle = 
    needle `isPrefixOf` haystack || xs `contains` needle

dirWalk :: FilePath -> IO [FilePath]
dirWalk dir = let
    isValid name = not $ ("/." `isSuffixOf` name) || ("/.." `isSuffixOf` name)
    listDirs = filterM doesDirectoryExist
    listFiles = filterM doesFileExist
    joinName base path = base ++ "/" ++ path
  in do
    noDots <- getDirectoryContents dir 
          >>= filterM (return . isValid) . map (joinName dir)
    subDirFiles <- listDirs noDots >>= mapM dirWalk >>= return . concat
    files <- listFiles noDots
    return $ files ++ subDirFiles

getTestFiles = dirWalk "../test" 
           >>= return 
               . (filter (not . flip contains ".svn"))
               . (filter (not . flip contains "~"))
               . (filter (not . flip contains ".swp"))
               . (filter (flip contains ".evetest"))
openTestFile filename = openFile filename ReadMode 
                    >>= hGetContents >>= return . lines

testLines :: String -> (String -> EveM String) -> [String] -> EveM ()
testLines filename fn [] = return ()
testLines filename fn (rawInput:output:rest) = 
    runTest `catchError` printError >> testLines filename fn rest
  where
    printError = liftIO . putStrLn . (("Error in " ++ filename ++ ": ") ++) . show
    input = drop 7 rawInput
    runTest = do
      result <- fn input
      if result == output
        then return ()
        else liftIO $ putStrLn $
                "Test " ++ filename ++ " failed: input " ++ input ++
                " evaluated to " ++ result ++ " instead of " ++ output
testLines filename fn _ = liftIO $ putStrLn ("Error in input format for file " ++ filename)

runTest filename = openTestFile filename 
                   >>= flip runEveM (primitiveEnv) 
                     . testLines filename f
  where 
    runLex input = lexer input >>= return . show . map showTok
    runParse input = lexer input >>= parseRepl >>= return. show
    runEval input = do
      env <- getEnv 
      lexer input >>= parseRepl >>= evalRepl env >>= return . show
    f = case () of 
          () | filename `contains` "lex" -> runLex
             | filename `contains` "parse" -> runParse
             | filename `contains` "eval" -> runEval
             | otherwise -> return
             
main = getTestFiles >>= mapM runTest
