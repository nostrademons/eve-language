module Eval(eval, evalRepl, autoImports) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO

import Data
import Utils
import Lexer
import Parser
import Primitives

autoImports = ["eve.data.range"]
-- autoImports = []

categorize (Import x) (i, b, d, t) = (x : i, b, d, t)
categorize x@(Binding _ _) (i, b, d, t) = (i, x : b, d, t)
categorize x@(Def _ _ _ _ _ _) (i, b, d, t) = (i, b, x : d, t)
categorize x@(TypeDef _ _) (i, b, d, t) = (i, b, d, x : t)

parseFileLines = foldr categorize ([], [], [], [])

parseDef (Def name args docstring typeDecl lines body) = (name, Lambda args convertedBody)
  where
    (_, bindings, defs, typedefs) = parseFileLines lines
    defBody = Letrec (map parseDef defs) body
    convertedBody = foldr convertBinding defBody bindings
    convertBinding (Binding var expr) rest = Funcall (Lambda [var] rest) [expr]

readModule :: String -> EveM ModuleDef
readModule fileText = do
    (imports, bindings, defs, typedefs) <- lexer fileText >>= parseFile 
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

-- Our tying the knot approach doesn't work here: the lookup call evaluates the
-- whole thing, which results in infinite recursion.  A better approach might
-- be to remove the multimethod type and instead, in apply, check for previous
-- bindings in the a-list and possibly execute them instead.  Then we no longer
-- need the lookup, and a simple tying the knot when we create a bunch of defs
-- will suffice.
evalLetrec :: Env -> [(String, EveExpr)] -> Env
evalLetrec env defs = result
  where
    result = map makeBinding defs
    makeBinding (name, Lambda args body) = (name, Function args body (result ++ env))

eval :: Env -> EveExpr -> EveM EveData
eval env (Literal val) = return val
eval env (TupleLiteral args) = mapM (eval env) args >>= return . Tuple
eval env (RecordLiteral args) = mapM evalRecord args >>= return . Record
  where evalRecord (label, expr) = 
            do value <- eval env expr
               return (label, value)
eval env (Variable var) = case multiLookup var env of
    [] -> throwError $ UnboundVar var
    options -> return $ if all isFunction options 
        then MultiMethod options else options !! 0
  where
    isFunction (Function _ _ _) = True
    isFunction (Primitive _ _) = True
    isFunction (MultiMethod _) = True
    isFunction _ = False
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
tryEvalRecordField env (Variable field) [argExpr] err@(UnboundVar _) = do
  value <- eval env argExpr
  case value of 
    Record fields -> maybe (throwError $ MissingField value field) return $
                    lookup field fields
    otherwise -> throwError err
tryEvalRecordField env _ _ err = throwError err

apply :: EveData -> [EveData] -> EveM EveData
apply (Primitive name fn) args = fn args
apply (Function argNames body env) args = if length argNames == length args
    then eval (zip argNames args ++ env) body
    else throwError $ TypeError $ "Wrong number of arguments: expected " 
                            ++ show argNames ++ ", found " ++ show args
apply (MultiMethod [single]) args = apply single args
apply (MultiMethod (method:rest)) args = apply method args `catchError` tryRest
  where
    tryRest _ = apply (MultiMethod rest) args
apply val args = throwError $ TypeError $ show val ++ " is not a function"
