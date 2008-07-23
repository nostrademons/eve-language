module Eval(eval, evalRepl, autoImports, startingEnv) where
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

startingEnv = [("apply", Primitive "apply" applyPrimitive)] ++ primitiveEnv

categorize (Import x) (i, b, d, t) = (x : i, b, d, t)
categorize x@(Binding _ _) (i, b, d, t) = (i, x : b, d, t)
categorize x@(Def _ _ _ _ _ _) (i, b, d, t) = (i, b, x : d, t)
categorize x@(TypeDef _ _) (i, b, d, t) = (i, b, d, x : t)

parseFileLines = foldr categorize ([], [], [], [])

parseDef typeEnv (Def name args docstring typeDecl lines body) = (name, convertedBody)
  where
    (_, bindings, defs, _) = parseFileLines lines
    defBody = Letrec (map (parseDef typeEnv) defs) body
    convertedBody = Lambda args $ 
        (maybe id (addTypeChecks . convertTypeDefs) typeDecl) $
        foldr convertBinding defBody bindings
    convertBinding (Binding (Left vars) expr) rest = 
        Funcall (Variable "apply") [Lambda vars rest, expr]
    convertBinding (Binding (Right var) expr) rest = Funcall (Lambda [var] rest) [expr]
    addTypeChecks (TFunc tArgs ret) = TypeCheck (zip args tArgs)
    convertTypeDefs (TPrim name) = maybe (TPrim name) id $ lookup name typeEnv
    convertTypeDefs (TTuple types) = TTuple $ map convertTypeDefs types
    convertTypeDefs (TRecord types) = TRecord $ zip keys $ map convertTypeDefs values
      where (keys, values) = unzip types
    convertTypeDefs (TFunc args ret) = TFunc (map convertTypeDefs args) $ convertTypeDefs ret

readModule :: String -> EveM ModuleDef
readModule fileText = do
    (imports, bindings, defs, typedefs) <- lexer fileText >>= parseFile 
                 >>= return . parseFileLines
    importEnv <- mapM loadModule imports >>= return . (startingEnv ++) . concat
    evalEnv <- foldl (>>=) (return importEnv) $ map evalBinding bindings
    defEnv <- return $ evalLetrec evalEnv $ map (parseDef $ map parseType typedefs) defs
    return $ take (length bindings) evalEnv ++ defEnv
  where
    parseType (TypeDef name val) = (name, val)
    -- TODO: sequence-unpacking top-level binding
    evalBinding (Binding (Right var) expr) env = do
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
eval env (TypeCheck types body) = mapM_ checkType types >> eval env body
  where
    checkType (varname, typeDecl) = eval env (Variable varname) >>= throwIfInvalid typeDecl
    throwIfInvalid (TPrim "Int") val@(Int _) = return val
    throwIfInvalid (TPrim "Bool") val@(Bool _) = return val
    throwIfInvalid (TPrim "Str") val@(String _) = return val
    throwIfInvalid (TPrim "Sym") val@(Symbol _) = return val
    -- TODO: function types
    throwIfInvalid (TTuple types) val@(Tuple fields) = checkAll types fields val
    throwIfInvalid (TRecord types) val@(Record fields) = 
        checkAll (extractVals types) (extractVals fields) val
    throwIfInvalid typeDecl val = throwError $ TypeError (show val ++ " is not a " ++ show typeDecl)
    checkAll types fields val = sequence_ (zipWith throwIfInvalid types fields) >> return val
    extractVals = snd . unzip . sortRecord

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

iterableValues :: EveData -> EveM [EveData]
iterableValues iter = do
    env <- getEnv
    Bool hasNext <- iterCall env "has_next"
    if hasNext then do
        val <- iterCall env "get"
        next <- iterCall env "next"
        rest <- iterableValues next
        return $ val : rest
      else
        return []
  where iterCall env name = eval env $ eveCall name [iter]

applyPrimitive :: [EveData] -> EveM EveData
applyPrimitive [fn, sequence] = do
    env <- getEnv
    iter <- eval env $ eveCall "iter" [sequence]
    values <- iterableValues iter
    apply fn $ values
