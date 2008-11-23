module Repl(runRepl, replOutput) where
import IO
import Control.Monad.Trans

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: MonadIO m => String -> m String
readPrompt prompt = liftIO $ flushStr ("\n" ++ prompt ++ ">>> ") >> getLine

until_ :: MonadIO m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

replOutput output = liftIO . putStr . show $ output

runRepl :: MonadIO m => String -> (String -> m ()) -> m ()
runRepl prompt action = until_ (== "quit") (readPrompt prompt) action
