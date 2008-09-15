module Main(main) where
import IO
import Control.Monad.Trans
import Control.Monad.Error hiding (join)
import System.FilePath.Posix
import Data.List

import Data
import Utils
import Lexer
import Parser
import Eval
import Primitives

filterExtensions ext = 
         (filter (not . flip contains ".svn"))
       . (filter (not . flip contains "~"))
       . (filter (not . flip contains ".swp"))
       . (filter (flip contains ("." ++ ext)))

getTestFiles = dirWalk "../test" >>= return . filterExtensions "evetest" 

testLines :: String -> (String -> String -> EveM String) -> [String] -> EveM ()
testLines filename fn [] = return ()
testLines filename fn (rawInput:output:rest) = runTest >> testLines filename fn rest
  where
    printError (StackTrace _ _ err) = return . show $ err
    (prompt, notPrompt) = span (/= '>') rawInput
    input = drop 4 notPrompt
    runTest = do
      result <- fn prompt input `catchError` printError
      if result == output
        then return ()
        else liftIO $ putStrLn $
                "Test " ++ filename ++ " failed on input " ++ input ++ 
                ": \nExpected: " ++ output ++ "\nFound:    " ++ result
testLines filename fn _ = liftIO $ putStrLn ("Error in input format for file " ++ filename)

evalLine filename line = lexer filename line >>= parseRepl >>= evalRepl

evalInitial = mapM_ evalImport autoImports
  where evalImport moduleName = evalLine moduleName $ "import " ++ moduleName

runTest filename = do
    text <- openTestFile filename
    env <- startingEnv
    runEveM (testLines filename f text) env
  where 
    runLex prompt input = lexer filename input >>= return . show . map showTok
    runParse :: String -> String -> EveM String
    runParse prompt input = lexer filename input >>= parser prompt
    runEval prompt input = do
        evalInitial >> lexer filename input >>= parseRepl >>= showAction evalRepl
    parseReplLine line = parseRepl line >>= return . showExpr
    parseFileLine line = parseFile line >>= return . showExpr
    parser :: String -> [(EveToken, SourcePos)] -> EveM String
    parser prompt = if prompt == "Eve" then parseReplLine else parseFileLine
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
    Right (parseTree, _) <- runEveM ((lexer filename text >>= parseFile) `catchError` (return . const [])) []
    return $ extractDocstrings parseTree

extractDocstrings :: [EveFileLine] -> [(String, String)]
extractDocstrings [] = []
extractDocstrings ((Def name argData docstring _ _ _, pos):xs) = 
    (name ++ "(" ++ show argData ++ ")", docstring) : extractDocstrings xs
extractDocstrings ((Class name _ (docstring, _), pos):xs) = 
    (name, docstring) : extractDocstrings xs
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
    runTests lines = liftIO startingEnv >>= runEveM (evalInitial >> mapM runTest lines) 
    runTest (test, expected) = do
        result <- (evalLine filename test >>= return . show) `catchError` (return . show)
        return $ if result == expected then Nothing else Just (test, expected, result)
    showFailure (Just (test, expected, found)) = "In " ++ moduleName ++ "." ++ testName ++ 
        ",\n  Testing: " ++ test ++ "\n  Expected: " ++ expected ++ "\n  Found: " ++ found

runDocTests filename = readDocStrings filename >>= mapM (printResults filename)

main = getTestFiles >>= mapM runTest >> getLibFiles >>= mapM runDocTests
