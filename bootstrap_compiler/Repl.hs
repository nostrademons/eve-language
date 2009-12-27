module Main(main) where
import System
import IO
import List
import Maybe
import Monad
import Control.Monad.Error
import Control.Monad.Trans

import Expr (FileLineValue(..), exprType, fileLineVal)
import Lexer
import Parser
import TypeCheck
import Error

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: MonadIO m => Bool -> String -> m String
readPrompt isContinuation prompt = liftIO $ do
    flushStr (if isContinuation then replicate (length prompt)'.' ++ "... " 
                                 else '\n' : prompt ++ ">>> ")
    line <- getLine
    if "\\" `isSuffixOf` line
        then liftM ((init line ++ "\n") ++) $ readPrompt True prompt
        else return line

until_ :: MonadIO m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
    result <- prompt
    if pred result 
        then return ()
        else action result >> until_ pred prompt action

replOutput output = liftIO . putStr . show $ output

runRepl :: MonadIO m => String -> (String -> m ()) -> m ()
runRepl prompt action = until_ (== "quit") (readPrompt False prompt) action

parseLine input = putStrLn output
  where
    Right output = (lexer "stdin" input >>= parseRepl >>=  printReplLine . fileLineVal)
                        `catchError` (return . show)

printReplLine (NakedExpr expr) = liftM show $ typeCheck expr
printReplLine other = return $ show other

main = runRepl "Eve" parseLine
