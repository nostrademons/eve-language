module Eval(eval, evalRepl) where
import Control.Monad.Error hiding (join)
import IO

import Data
import Lexer
import Parser
import Primitives
import TypeCheck

evalRepl env (Expr expr) = eval env expr
evalRepl env (ReplImport path) = loadModule readModule path
           >>= liftM head . mapM addBinding . getAccessibleBindings ""
  where
    addBinding (var, value, valType, _) = 
        addTopLevelBinding var value valType >> return value
evalRepl env (Assignment var expr) = do
  value <- eval env expr
  tEnv <- getTypes
  valType <- typeCheck tEnv expr
  addTopLevelBinding var value valType
  return value

eval :: Env -> EveExpr -> EveM EveData
eval env (Literal val) = return val
eval env (ListLiteral args) = mapM (eval env) args >>= return . List
eval env (Variable var) = maybe (throwError $ UnboundVar var) return $
                            lookup var env
eval env (Funcall fnExpr argExpr) = do
  fn <- eval env fnExpr
  args <- mapM (eval env) argExpr
  apply fn args
eval env (Cond ((pred, action):rest)) = do
  predResult <- eval env pred
  if predResult == Bool True then eval env action else eval env (Cond rest)
eval env (Lambda args body) = return $ Function args body env

apply :: EveData -> [EveData] -> EveM EveData
apply (Primitive name fn) args = return $ fn args
apply (Function argNames body env) args = eval (zip argNames args ++ env) body

readModule :: [String] -> EveM ModuleDef
readModule path = fileText >>= lexer >>= parseFile >>= mapM makeBinding
  where
    filename = "../src/" ++ join "/" path ++ ".eve"
    fileText = liftIO $ openFile filename ReadMode >>= hGetContents 
    makeBinding (Binding var expr) = do
      exprType <- typeCheck primitiveTypes expr
      exprVal <- eval primitiveEnv expr
      return (var, exprVal, exprType, "")