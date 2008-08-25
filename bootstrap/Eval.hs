module Eval(eval, evalRepl, autoImports, startingEnv) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO

import Data
import Utils
import Lexer
import Parser
import Primitives

split :: Char -> String -> [String]
split delim s
    | [] <- rest = [token]
    | otherwise = token : split delim (tail rest)
  where (token,rest) = span (/=delim) s

autoImports = ["eve.lang.functions", "eve.data.iterator", "eve.data.range"]
-- autoImports = []

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
    ("_attr", attrRawPrimitive),
    ("attr", attrPrimitive),
    ("restrict", filterRecord elem),
    ("exclude", filterRecord notElem)]

categorize (NakedExpr _) val = val
categorize (Import x) (i, b, d, t) = (x : i, b, d, t)
categorize x@(Binding _ _) (i, b, d, t) = (i, x : b, d, t)
categorize x@(Def _ _ _ _ _ _) (i, b, d, t) = (i, b, x : d, t)
categorize x@(Class _ _ _) (i, b, d, t) = (i, b, x : d, t)
categorize x@(TypeDef _ _) (i, b, d, t) = (i, b, d, x : t)

parseFileLines = foldr categorize ([], [], [], [])

parseDef :: TEnv -> EveFileLine -> (String, EveExpr)
parseDef typeEnv (Def name argData docstring typeDecl lines body) = (name, convertedBody)
  where
    (_, bindings, defs, _) = parseFileLines lines
    convertedBody = Lambda argData $ 
        (maybe id (addTypeCheck . convertTypeDefs) typeDecl) $
        foldr convertBinding defBody bindings
    defBody = Letrec (map (parseDef typeEnv) defs) body
    convertBinding (Binding (Left vars) expr) rest = 
        Funcall (Variable "apply") [Lambda (ArgExpr vars [] Nothing) rest, expr]
    convertBinding (Binding (Right var) expr) rest = Funcall (Lambda (ArgExpr [var] [] Nothing) rest) [expr]
    addTypeCheck typeDecl body = TypeCheck (body, typeDecl) body
    convertTypeDefs :: EveType -> EveType
    convertTypeDefs (TPrim name) = maybe (TPrim name) id $ lookup name typeEnv
    convertTypeDefs (TLiteral datum) = TLiteral datum
    convertTypeDefs (TTuple types) = TTuple $ map convertTypeDefs types
    convertTypeDefs (TRecord types) = TRecord $ zip keys $ map convertTypeDefs values
      where (keys, values) = unzip types
    convertTypeDefs (TFunc args ret) = TFunc (map convertTypeDefs args) $ convertTypeDefs ret

evalDef :: Env -> TEnv -> EveFileLine -> EveM (String, EveData)
evalDef env tEnv def@(Def _ _ _ _ _ _) = evalPair env $ parseDef tEnv def
evalDef env tEnv (Class name superDecl (docstring, lines)) = do
    methods <- mapM (evalPair env) methodExprs
    constr <- constructor methods
    return (name, constr)
  where
    constructor methods = do
        oldProto <- getAttr "proto" baseFunc
        return $ setAttributes baseFunc (("proto", oldProto) : methods)
      where baseFunc = makeFunction (Args [] [] (Just "args")) classBody (methods ++ env)
    funcall name args = Funcall (Variable name) args
    classBody = funcall "extend" [funcall "apply" [Variable "init", Variable "args"], proto]
    proto = RecordLiteral [("proto", RecordLiteral $ buildProto superDecl)]
    buildProto (Just superclass) = ("proto", Variable superclass) : methodExprs
    buildProto Nothing = methodExprs
    isDef (Def _ _ _ _ _ _) = True
    isDef _ = False
    methodExprs = map (parseDef tEnv) $ filter isDef lines

readModule :: String -> String -> EveM ModuleDef
readModule moduleName fileText = do
    (imports, bindings, defs, typeDefs) <- lexer fileText >>= parseFile 
                 >>= return . parseFileLines
    modules <- getStateField modules 
    importEnv <- mapM loadModule imports >>= return . ((startingEnv ++ autoImportEnv modules) ++) . concat
    evalEnv <- foldl (>>=) (return importEnv) $ map evalBinding bindings
    defResults <- mapM (evalDef evalEnv $ map parseType typeDefs) defs
    defEnv <- return $ closeOverBindings defResults
    return $ take (length bindings) evalEnv ++ defEnv
  where
    parseType (TypeDef name val) = (name, val)
    autoImportEnv :: [(String, ModuleDef)] -> Env
    autoImportEnv modules = concatMap (lookupModule modules) autoImports
    lookupModule :: [(String, ModuleDef)] -> String -> ModuleDef
    lookupModule moduleDefs name = 
        maybe (error $ "Module " ++ moduleName ++ " not loaded") id $ lookup name moduleDefs
    -- TODO: sequence-unpacking top-level binding
    evalBinding (Binding (Right var) expr) env = do
      datum <- eval env expr
      return $ (var, datum) : env 

loadModule :: [String] -> EveM ModuleDef
loadModule path = getStateField modules >>= maybeLoad
  where
    moduleName = join "." path
    maybeLoad modules = maybe firstTimeLoad return $ lookup moduleName modules
    firstTimeLoad = do
      filename <- return $ "../src/" ++ join "/" path ++ ".eve"
      fileText <- liftIO $ openFile filename ReadMode >>= hGetContents 
      moduleDef <- readModule moduleName fileText
      modify $ addModule moduleDef
      return moduleDef
    addModule moduleDef state = 
        state { modules = (moduleName, moduleDef) : modules state}

evalRepl env (Expr expr) = eval env expr
evalRepl env (ReplImport path) = loadModule path >>= liftM head . mapM addBinding 
  where
    addBinding (var, value) = 
        addTopLevelBinding var value >> return value
evalRepl env (Assignment var expr) = do
  value <- eval env expr
  addTopLevelBinding var value
  return value

closeOverBindings bindings = result
  where
    result = map editFunctionEnv bindings
    editFunctionEnv (name, (Function argData body env fields)) = 
        (name, Function argData body (result ++ env) fields)

evalDefaults env (ArgExpr args defaults varargs) = do
    values <- mapM (eval env) defaultExprs
    return $ Args args (zip names values) varargs
  where
    (names, defaultExprs) = unzip defaults

evalPair env (name, body) = do
    result <- eval env body
    return (name, result)

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
eval env (Lambda argExpr body) = do
    argData <- evalDefaults env argExpr
    return $ makeFunction argData body env
eval env (Letrec bindings body) = do
    fns <- mapM (evalPair env) bindings
    eval (closeOverBindings fns ++ env) body
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
apply (Function argData body env _) args = do
    boundArgs <- bindArgs argData args
    eval (boundArgs ++ env) body
  where
    bindArgs (Args argNames defaults varargs) args = case numArgs of
        numArgs | numArgs == numProvided -> return $ bindVarArgs varargs $ boundArgs
        numArgs | numArgs < numProvided && hasVarArgs -> return $ bindVarArgs varargs $ boundArgs
        numArgs | numArgs > numProvided && numArgs - numProvided <= length defaults ->
            return $ bindVarArgs varargs $ defaultsTaken 
                ++ zip (filter (flip notElem $ fst $ unzip defaultsTaken) argNames) args
        numArgs -> throwError $ TypeError $ "Wrong number of arguments: expected " ++ show numArgs
      where
        boundArgs = zip argNames args
        hasVarArgs = maybe False (const True) varargs
        bindVarArgs Nothing = id
        bindVarArgs (Just varargs) = ((varargs, makeTuple $ drop numArgs args) :)
        numArgs = length argNames
        numProvided = length args
        numDefaults = numArgs - numProvided
        defaultsTaken = take numDefaults defaults
apply val args = throwError $ TypeError $ show val ++ " is not a function"

eveMethodCall :: EveData -> String -> [EveData] -> EveM EveData
eveMethodCall obj name args = getAttr name obj >>= flip apply (obj : args)

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
  where iterCall env name = eveMethodCall iter name []

sequenceValues :: EveData -> EveM [EveData]
sequenceValues sequence = do
    env <- getEnv 
    eveMethodCall sequence "iter" [] >>= iterableValues

applyPrimitive :: [EveData] -> EveM EveData
applyPrimitive [fn, sequence] = sequenceValues sequence >>= apply fn
applyPrimitive _ = throwError $ TypeError $ "Apply takes a function and a sequence"

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

attrPrimitive [obj, field@(String name _)] = tryRecord `catchError` tryAttr
  where
    tryRecord = getAttr name obj >>= return . maybeMakeMethod
    tryAttr e | hasAttr "attr" obj = getAttr "attr" obj >>= flip apply [obj, field]
    tryAttr e = throwError e
    wrappedFunction fn env = Function (Args [] [] (Just "args")) 
        (Funcall (Variable "apply") [Literal fn, 
            Funcall (Variable "add") [Literal $ makeTuple [obj], Variable "args"]]) 
        env [("im_self", obj), ("im_func", fn)]
    maybeMakeMethod fn@(Primitive _ _ _) = wrappedFunction fn startingEnv
    maybeMakeMethod fn@(Function (Args [] _ _) _ _ _) = fn
    maybeMakeMethod fn@(Function _ _ env _) = wrappedFunction fn env
    maybeMakeMethod val = val
attrPrimitive _ = throwError $ TypeError "Field access requires an object and a string"

attrRawPrimitive [obj, String field _] = getAttr field obj
attrRawPrimitive _ = throwError $ TypeError "Field access requires an object and a string"

filterRecord fn [dest, fields] = trySequence `catchError` const tryFields
  where
    trySequence = sequenceValues fields >>= mapM extractString >>= modifyFields
    tryFields = modifyFields . attrNames $ fields
    extractString (String val _) = return val
    extractString _ = throwError $ TypeError "Second element of restrict must be sequence of strings"
    modifyFields :: [String] -> EveM EveData
    modifyFields source = return . setAttributes dest . 
                    filter (\(key, val) -> fn key source) $ attributes dest
