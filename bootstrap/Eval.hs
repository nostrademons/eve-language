module Eval(eval, evalRepl, autoImports, startingEnv) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO

import Data
import Utils
import Lexer
import Parser
import Primitives

-- autoImports = ["eve.data.range"]
autoImports = []

startingEnv = primitiveEnv ++ makePrimitives [
    ("apply", applyPrimitive),
    ("add", binopPrimitive "add"),
    ("sub", binopPrimitive "sub"),
    ("pow", binopPrimitive "pow"),
    ("mul", binopPrimitive "mul"),
    ("div", binopPrimitive "div"),
    ("mod", binopPrimitive "mod"),
    ("eq", cmpPrimitive "eq"),
    ("ne", cmpPrimitive "ne"),
    ("gt", cmpPrimitive "gt"),
    ("ge", cmpPrimitive "ge"),
    ("lt", cmpPrimitive "lt"),
    ("le", cmpPrimitive "le"),
    ("extend", extendPrimitive),
    ("attr", attrPrimitive),
    ("restrict", filterRecord elem),
    ("exclude", filterRecord notElem)]

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
        (maybe id (addTypeCheck . convertTypeDefs) typeDecl) $
        foldr convertBinding defBody bindings
    addTypeCheck typeDecl body = TypeCheck (body, typeDecl) body
    convertBinding (Binding (Left vars) expr) rest = 
        Funcall (Variable "apply") [Lambda vars rest, expr]
    convertBinding (Binding (Right var) expr) rest = Funcall (Lambda [var] rest) [expr]
    convertTypeDefs :: EveType -> EveType
    convertTypeDefs (TPrim name) = maybe (TPrim name) id $ lookup name typeEnv
    convertTypeDefs (TLiteral datum) = TLiteral datum
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
    makeBinding (name, Lambda args body) = (name, makeFunction args body (result ++ env))

eval :: Env -> EveExpr -> EveM EveData
eval env (Literal val) = return val
eval env (TupleLiteral args) = mapM (eval env) args >>= return . makeTuple
eval env (RecordLiteral args) = mapM evalRecord args >>= return . Record
  where evalRecord (label, expr) = 
            do value <- eval env expr
               return (label, value)
eval env (Variable var) = maybe (throwError $ UnboundVar var) return $ lookup var env
eval env (Funcall fnExpr argExpr) = do
  fn <- eval env fnExpr
  args <- mapM (eval env) argExpr
  apply fn args
eval env (Cond ((pred, action):rest)) = do
  predResult <- eval env pred
  case predResult of
    Bool True _ -> eval env action 
    otherwise -> eval env (Cond rest)
eval env (Lambda args body) = return $ makeFunction args body env
eval env (Letrec bindings body) = eval (evalLetrec env bindings ++ env) body
eval env (TypeCheck (tested, typeDecl) body) = 
    eval env tested >>= throwIfInvalid typeDecl >> eval env body
  where
    throwIfInvalid (TPrim "Int") val@(Int _ _) = return val
    throwIfInvalid (TPrim "Bool") val@(Bool _ _) = return val
    throwIfInvalid (TPrim "Str") val@(String _ _) = return val
    throwIfInvalid (TPrim "Sym") val@(Symbol _ _) = return val
    throwIfInvalid (TLiteral expected) val | val == expected = return val
    -- TODO: function types
    throwIfInvalid (TTuple types) val@(Tuple fields _) = checkAll types fields val
    throwIfInvalid (TRecord types) val@(Record fields) = 
        checkAll (extractVals types) (extractVals $ recordFields fields) val
    throwIfInvalid typeDecl val = throwError $ TypeError (show val ++ " is not a " ++ show typeDecl)
    checkAll types fields val = sequence_ (zipWith throwIfInvalid types fields) >> return val
    extractVals = snd . unzip . sortRecord

apply :: EveData -> [EveData] -> EveM EveData
apply (Primitive name fn _) args = fn args
apply (Function argNames body env _) args = if length argNames == length args
    then eval (zip argNames args ++ env) body
    else throwError $ TypeError $ "Wrong number of arguments: expected " 
                            ++ show argNames ++ ", found " ++ show args
apply val args = throwError $ TypeError $ show val ++ " is not a function"

iterableValues :: EveData -> EveM [EveData]
iterableValues iter = do
    env <- getEnv
    Bool hasNext _ <- iterCall env "has_next"
    if hasNext then do
        val <- iterCall env "get"
        next <- iterCall env "next"
        rest <- iterableValues next
        return $ val : rest
      else
        return []
  where iterCall env name = eval env $ eveCall name [iter]

sequenceValues :: EveData -> EveM [EveData]
sequenceValues sequence = getEnv >>= (flip eval $ eveCall "iter" [sequence]) >>= iterableValues

applyPrimitive :: [EveData] -> EveM EveData
applyPrimitive [fn, sequence] = sequenceValues sequence >>= apply fn

binopPrimitive name [left, right] = tryLeftAttr `catchError` const tryRightAttr 
  where
    tryLeftAttr = getAttr name left >>= flip apply [left, right]
    tryRightAttr = getAttr ('r' : name) right >>= flip apply [right, left]

cmpPrimitive name [left, right] = tryMethod 
        `catchError` const tryReflection 
        `catchError` const tryCmp
  where 
    reflections = [
        ("eq", ("eq", (== 0))), ("ne", ("ne", (/= 0))),
        ("gt", ("le", (> 0))), ("le", ("gt", (<= 0))),
        ("lt", ("ge", (< 0))), ("ge", ("lt", (>= 0)))]
    (ref, cmpPred) = maybe (error (name ++ " not a cmp function")) id $ lookup name reflections
    tryMethod = getAttr name left >>= flip apply [left, right]
    tryReflection = getAttr ref right >>= flip apply [right, left]
    tryCmp = getAttr "cmp" left >>= flip apply [left, right] >>= cmpResult
    cmpResult (Int num _) = return $ if cmpPred num then makeBool True else makeBool False
    cmpResult _ = throwError $ TypeError "cmp should return an int"

extendPrimitive [dest, source] = return . setAttributes dest $ 
        (attributes source ++ dropAttrs (attrNames source) dest)
  where
    exclude dest source = filter (\(key, val) -> key `notElem` source) dest

attrPrimitive [obj, String field _] = getAttr field obj
attrPrimitive _ = throwError $ TypeError "Field access requires an object and a string"

filterRecord fn [dest, fields] = trySequence `catchError` const tryFields
  where
    trySequence = sequenceValues fields >>= mapM extractString >>= modifyFields
    tryFields = modifyFields . attrNames $ fields
    extractString (String val _) = return val
    extractString _ = throwError $ TypeError "Second element of restrict must be sequence of strings"
    modifyFields :: [String] -> EveM EveData
    modifyFields source = return . setAttributes dest . 
                    filter (\(key, val) -> fn key source) $ attributes dest
