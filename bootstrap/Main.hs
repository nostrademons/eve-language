module Main(main) where
import System.Environment hiding (getEnv)
import Control.Monad.State
import Control.Monad.Error
import IO
import Data
import Lexer
import Parser
import Eval
import TypeCheck
import Repl
import Primitives

doPhases input = do
     env <- getEnv
     tEnv <- getTypes
     lexOutput <- lexer input
     parseOutput <- parseRepl lexOutput
     evalOutput <- evalRepl env parseOutput
     typeOutput <- runIfExpr (typeCheck tEnv) parseOutput
     return (lexOutput, parseOutput, evalOutput, typeOutput)

replOutput evalOutput typeOutput = do
     liftIO . putStrLn $ "Type: " ++ show typeOutput
     liftIO . putStr . show $ evalOutput

saveAndPrint files input = do
     (lexOutput, parseOutput, evalOutput, typeOutput) <- doPhases input
     mapM_ writeOutput $ zip files
        [show $ map showTok lexOutput, 
         show parseOutput, 
         show evalOutput,
         show typeOutput]
     replOutput evalOutput typeOutput
  where
    writeOutput (file, result) = liftIO $
        hPutStrLn file ("Eve>>> " ++ input ++ "\n" ++ result) >> hFlush file

printOutput input = do
    (_, _, evalOutput, typeOutput) <- doPhases input
    replOutput evalOutput typeOutput

handleError action = flip catchError (liftIO . print) . action

main = let 
    repl = runRepl "Eve" 
    initialState = (primitiveEnv, primitiveTypes)
    makeFile name outType = liftIO $ openFile 
         ("../test/" ++ outType ++ "_" ++ name ++ ".evetest") AppendMode
  in do
    args <- getArgs 
    if length args == 0 
      then runEveM (repl $ handleError printOutput) initialState >> return ()
      else do 
        files <- mapM (makeFile (args !! 0)) ["lex", "parse", "eval", "types"]
        runEveM (repl $ handleError $ saveAndPrint files) initialState
        mapM_ hClose files
