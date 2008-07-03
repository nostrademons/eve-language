module Main(main) where
import IO
import Monad hiding (join)
import Control.Monad.Trans
import Control.Monad.Error hiding (join)
import Directory
import System.FilePath.Posix
import Data.List

import Data
import Utils
import Lexer
import Parser
import Eval
import Primitives

-- For some reason, isInfixOf isn't in my version of GHC.
contains [] needle = False
contains haystack@(x:xs) needle = 
    needle `isPrefixOf` haystack || xs `contains` needle

stripTo haystack needle = 
    drop (length (dropWhile (not . isSuffixOf needle) (inits haystack) !! 0)) haystack

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

filterExtensions ext = 
         (filter (not . flip contains ".svn"))
       . (filter (not . flip contains "~"))
       . (filter (not . flip contains ".swp"))
       . (filter (flip contains ("." ++ ext)))

getTestFiles = dirWalk "../test" >>= return . filterExtensions "evetest" 

testLines :: String -> (String -> String -> EveM String) -> [String] -> EveM ()
testLines filename fn [] = return ()
testLines filename fn (rawInput:output:rest) = 
    runTest `catchError` printError >> testLines filename fn rest
  where
    printError = liftIO . putStrLn . (("Error in " ++ filename ++ ": ") ++) . show
    (prompt, notPrompt) = span (/= '>') rawInput
    input = drop 4 notPrompt
    runTest = do
      result <- fn prompt input
      if result == output
        then return ()
        else liftIO $ putStrLn $
                "Test " ++ filename ++ " failed on input " ++ input ++ 
                ": \nExpected: " ++ output ++ "\nFound:    " ++ result
testLines filename fn _ = liftIO $ putStrLn ("Error in input format for file " ++ filename)

runTest filename = openTestFile filename 
                   >>= flip runEveM (primitiveEnv) 
                     . testLines filename f
  where 
    runLex prompt input = lexer input >>= return . show . map showTok
    runParse :: String -> String -> EveM String
    runParse prompt input = 
      lexer input 
        >>= (if prompt == "Eve" then showAction parseRepl else showAction parseFile) 
        >>= return
    runEval prompt input = do
      env <- getEnv 
      lexer input >>= parseRepl >>= showAction (evalRepl env)
    showAction action arg = action arg >>= return . show
    f :: String -> String -> EveM String
    f = case () of 
          () | filename `contains` "lex" -> runLex
             | filename `contains` "parse" -> runParse
             | filename `contains` "eval" -> runEval
             | otherwise -> \prompt input -> return input
             
getLibFiles = dirWalk "../src" >>= return . filterExtensions "eve"

readDocStrings filename = do
    text <- openFile filename ReadMode >>= hGetContents
    Right (parseTree, _) <- runEveM ((lexer text >>= parseFile) `catchError` (return . const [])) []
    return $ extractDocstrings parseTree

extractDocstrings :: [EveFileLine] -> [(String, String)]
extractDocstrings [] = []
extractDocstrings ((Def name args docstring _ _):xs) = 
    (name ++ "(" ++ join ", " args ++ ")", docstring) : extractDocstrings xs
extractDocstrings (_:xs) = extractDocstrings xs

extractTests :: [String] -> [(String, String)]
extractTests [] = []
extractTests (x:xs)
  | x `contains` ">>>" = (x `stripTo` ">>> ", dropWhile (== ' ') $ head xs) : extractTests (tail xs)
  | otherwise = extractTests(xs)

printResults :: String -> (String, String) -> IO [()]
printResults filename (testName, docstring) = do
    Right (failures, _) <- return (extractTests (lines docstring)) >>= runTests
    mapM (putStrLn . showFailure) $ filter (/= Nothing) failures
  where
    moduleName = join "." $ drop 2 $ splitDirectories $ dropExtension $ filename 
    runTests lines = runEveM (evalInitial >> mapM runTest lines) primitiveEnv
    runTest (test, expected) = do
        result <- (evalLine test >>= return . show) `catchError` (return . show)
        return $ if result == expected then Nothing else Just (test, expected, result)
    evalInitial = evalLine ("import " ++ moduleName)
    evalLine line = do
        env <- getEnv
        lexer line >>= parseRepl >>= evalRepl env
    showFailure (Just (test, expected, found)) = "In " ++ moduleName ++ "." ++ testName ++ 
        ",\n  Testing: " ++ test ++ "\n  Expected: " ++ expected ++ "\n  Found: " ++ found

runDocTests filename = readDocStrings filename >>= mapM (printResults filename)

main = getTestFiles >>= mapM runTest >> getLibFiles >>= mapM runDocTests
