module Main(main) where
import System.Environment hiding (getEnv)
import Control.Monad.State
import Control.Monad.Error
import IO
import Data
import Lexer
import Parser
import Eval
import Repl
import Primitives

doPhases input = do
     env <- getEnv
     lexOutput <- lexer input
     parseOutput <- parseRepl lexOutput
     evalOutput <- evalRepl env parseOutput
     return (lexOutput, parseOutput, evalOutput)

replOutput evalOutput = do
     liftIO . putStrLn . show $ evalOutput

saveAndPrint files input = do
     (lexOutput, parseOutput, evalOutput) <- doPhases input
     mapM_ writeOutput $ zip files
        [show $ map showTok lexOutput, 
         show parseOutput, 
         show evalOutput]
     replOutput evalOutput 
  where
    writeOutput (file, result) = liftIO $
        hPutStr file ("Eve>>> " ++ input ++ "\n" ++ result) >> hFlush file

printOutput input = do
    (lexOutput, parseOutput, evalOutput) <- doPhases input
    replOutput evalOutput 

handleError action = flip catchError (liftIO . print) . action

main = let 
    repl = runRepl "Eve" 
    initialState = (primitiveEnv)
    makeFile name outType = liftIO $ openFile 
         ("../test/" ++ outType ++ "_" ++ name ++ ".evetest") AppendMode
  in do
    args <- getArgs 
    if length args == 0 
      then runEveM (repl $ handleError printOutput) initialState >> return ()
      else do 
        files <- mapM (makeFile (args !! 0)) ["lex", "parse", "eval"]
        runEveM (repl $ handleError $ saveAndPrint files) initialState
        mapM_ hClose files
