module Eval(eval, evalRepl) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO

import Data
import Lexer
import Parser
import Primitives

-- New approach: after parsing the module, we collect all the imports, then all
-- the bindings and other expressions, then all the defs.  We run the imports,
-- then execute all bindings sequentially in the environment formed by those
-- imports.  Once we have those bindings, we run the defs to close over the
-- environment created by those bindings, plus the defs themselves (we need
-- to do a bit of tying the knot, passing in the result of building the defs
-- into the defs themselves for inclusion as the closure.)

categorize (Import x) (i, b, d) = (x : i, b, d)
categorize x@(Binding _ _) (i, b, d) = (i, x : b, d)
categorize x@(Def _ _ _ _) (i, b, d) = (i, b, x : d)

parseFileLines = foldr categorize ([], [], [])

parseDef (Def name args lines body) = (name, Lambda args convertedBody)
  where
    (_, bindings, defs) = parseFileLines lines
    defBody = Letrec (map parseDef defs) body
    convertedBody = foldr convertBinding defBody bindings
    convertBinding (Binding var expr) rest = Funcall (Lambda [var] rest) [expr]

readModule :: String -> EveM ModuleDef
readModule fileText = do
    (imports, bindings, defs) <- lexer fileText >>= parseFile 
                             >>= return . parseFileLines
    importEnv <- mapM loadModule imports >>= return . (primitiveEnv ++) . concat
    evalEnv <- foldl (>>=) (return importEnv) $ map evalBinding bindings
    defEnv <- return $ evalLetrec evalEnv $ map parseDef defs
    return $ take (length bindings) evalEnv ++ defEnv
  where
    evalBinding (Binding var expr) env = do
      datum <- eval env expr
      return $ (var, datum) : env 

loadModule :: [String] -> EveM ModuleDef
loadModule path = getStateField modules >>= maybeLoad
  where
    moduleName = join "." path
    addModule moduleDef state = 
        state { modules = (moduleName, moduleDef) : modules state}
    maybeLoad modules = maybe firstTimeLoad return $ lookup moduleName modules
    firstTimeLoad = do
      filename <- return $ "../src/" ++ join "/" path ++ ".eve"
      fileText <- liftIO $ openFile filename ReadMode >>= hGetContents 
      moduleDef <- readModule fileText
      modify $ addModule moduleDef
      return moduleDef

evalRepl env (Expr expr) = eval env expr
evalRepl env (ReplImport path) = loadModule path >>= liftM head . mapM addBinding 
  where
    addBinding (var, value) = 
        addTopLevelBinding var value >> return value
evalRepl env (Assignment var expr) = do
  value <- eval env expr
  addTopLevelBinding var value
  return value

evalLetrec :: Env -> [(String, EveExpr)] -> Env
evalLetrec env defs = result
  where
    result = map makeBinding defs
    -- Anything other than a Lambda is a compiler error
    newFn (Lambda args body) = Function args body result
    makeBinding (name, expr) = (name, maybe (newFn expr) (addMethod expr) $ lookup name result)
    addMethod expr fn@(Function _ _ _) = MultiMethod [newFn expr, fn]
    addMethod expr (MultiMethod fns) = MultiMethod (newFn expr : fns)

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
eval env (Letrec bindings body) = eval (evalLetrec env bindings ++ env) body

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
