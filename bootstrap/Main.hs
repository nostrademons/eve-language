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

replAction input = do
    env <- getEnv
    lexer "top-level" input >>= parseRepl >>= evalRepl env

printOutput input = replAction input >>= replOutput

handleError action = flip catchError (liftIO . print) . action

main = let setup = handleError $ const $ mapM_ (replAction . ("import " ++)) autoImports
  in runEveM (setup "" >> runRepl "Eve" (handleError printOutput)) (startingEnv) >> return ()
