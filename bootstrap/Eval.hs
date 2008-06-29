module Eval(eval, evalRepl) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO

import Data
import Lexer
import Parser
import Primitives

loadModule :: [String] -> EveM ModuleDef
loadModule path = getStateField modules >>= maybeLoad
  where
    moduleName = join "." path
    addModule moduleDef state = 
        state { modules = (moduleName, moduleDef) : modules state}
    maybeLoad modules = maybe firstTimeLoad return $ lookup moduleName modules
    firstTimeLoad = do
      moduleDef <- readModule path
      modify $ addModule moduleDef
      return moduleDef
    readModule path = fileText >>= lexer >>= parseFile 
                    >>= mapM parseLine >>= mapM makeModuleDef
      where
        filename = "../src/" ++ join "/" path ++ ".eve"
        fileText = liftIO $ openFile filename ReadMode >>= hGetContents 
        makeModuleDef (var, exprVal) = return (var, exprVal, "")
        makeBinding var expr = do
          exprVal <- eval primitiveEnv expr
          return (var, exprVal)
        parseLine (Binding var expr) = makeBinding var expr
        parseLine (Def name args bindings body) = 
            makeBinding name $ Lambda args $ makeFn bindings body
        makeFn [] body = body
        makeFn (Binding var expr : rest) body = 
            Funcall (Lambda [var] (makeFn rest body)) [expr]
        -- TODO: all the other cases


evalRepl env (Expr expr) = eval env expr
evalRepl env (ReplImport path) = loadModule path
           >>= liftM head . mapM addBinding . getAccessibleBindings ""
  where
    addBinding (var, value, _) = 
        addTopLevelBinding var value >> return value
evalRepl env (Assignment var expr) = do
  value <- eval env expr
  addTopLevelBinding var value
  return value

eval :: Env -> EveExpr -> EveM EveData
eval env (Literal val) = return val
eval env (TupleLiteral args) = mapM (eval env) args >>= return . Tuple
eval env (RecordLiteral args) = mapM evalRecord args >>= return . Record
  where evalRecord (label, expr) = 
            do value <- eval env expr
               return (label, value)
eval env (Variable var) = maybe (throwError $ UnboundVar var) return $
                            lookup var env
eval env (Funcall fnExpr argExpr) = 
    tryEvalFuncall env fnExpr argExpr `catchError` tryEvalRecordField env fnExpr argExpr
eval env (Cond ((pred, action):rest)) = do
  predResult <- eval env pred
  if predResult == Bool True then eval env action else eval env (Cond rest)
eval env (Lambda args body) = return $ Function args body env

tryEvalFuncall env fnExpr argExpr = do
  fn <- eval env fnExpr
  args <- mapM (eval env) argExpr
  apply fn args
tryEvalRecordField env (Variable field) [argExpr] err = do
  value <- eval env argExpr
  case value of 
    Record fields -> maybe (throwError $ MissingField value field) return $
                    lookup field fields
    otherwise -> throwError err
tryEvalRecordField env _ _ err = throwError err

apply :: EveData -> [EveData] -> EveM EveData
apply (Primitive name fn) args = fn args
apply (Function argNames body env) args = eval (zip argNames args ++ env) body
apply (MultiMethod [single]) args = apply single args
apply (MultiMethod (method:rest)) args = apply method args `catchError` tryRest
  where
    tryRest _ = apply (MultiMethod rest) args
apply val args = throwError $ TypeError $ show val ++ " is not a function"
