module Eval(eval, evalRepl, autoImports, startingEnv) where
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)
import IO
import Data.List

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

autoImports = ["eve.lang.functions", "eve.data.iterator", "eve.data.range", "eve.data.list"]
-- autoImports = []

startingEnv = primitiveEnv ++ makePrimitives [
    ("apply", applyPrimitive),
    ("Record", convertToRecord),
    ("Tuple", convertToTuple),
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

funcall name args pos = (Funcall (Variable name, pos) args, pos)

parseFileLines [] = ([], [], [])
parseFileLines ((Import x, _) : rest) = (x : imports, typedefs, lines) 
  where (imports, typedefs, lines) = parseFileLines rest
parseFileLines (line@(TypeDef name typedef, _) : rest) = 
    (imports, (name, typedef) : typedefs, lines)
  where (imports, typedefs, lines) = parseFileLines rest
parseFileLines ((line@(Def _ _ _ _ def _), pos) : rest) = 
    (defImports ++ imports, defTypes ++ typedefs, newDef : lines)
  where 
    (defImports, defTypes, newLines) = parseFileLines def
    (imports, typedefs, lines) = parseFileLines rest
    newDef = (line { def_lines = newLines }, pos)
parseFileLines ((line@(Class _ _ (doc, cls)), pos) : rest) = 
    (classImports ++ imports, classTypes ++ typedefs, newClass : lines)
  where 
    (classImports, classTypes, newLines) = parseFileLines cls
    (imports, typedefs, lines) = parseFileLines rest
    newClass = (line { def_doc_lines = (doc, newLines) }, pos)
parseFileLines (line : rest) = (imports, typedefs, line : lines)
  where (imports, typedefs, lines) = parseFileLines rest

findVarNames lines = concatMap varNames lines
  where
    varNames (Binding (Left vars) _, _) = vars
    varNames (Binding (Right var) _, _) = [var]
    varNames (Def fnName _ _ _ _ _, _) = [fnName]
    varNames (Class className _ _, _) = [className]
    varNames _ = []

convertBindings bindings body = foldr convertBinding body bindings
  where
    convertBinding (Binding (Left vars) expr, pos) rest = 
        (Funcall (Variable "apply", pos) [(Lambda (ArgExpr vars [] Nothing) 
                                    False rest, pos), expr], pos)
    convertBinding (Binding (Right var) expr, pos) rest = 
        (Funcall (Lambda (ArgExpr [var] [] Nothing) False rest, pos) [expr], pos)
    convertBinding _ rest = rest

convertDefs typeEnv defs body = (Letrec (map convertDef $ filter isDef defs) body, snd body)
  where
    isDef (Def {}, pos) = True
    isDef (Class {}, pos) = True
    isDef _ = False
    convertDef def@(val, pos) = (def_name val, parseDef typeEnv def)

methodRecord lines = map makeMethod $ zip (findVarNames lines) $ map snd lines
  where
    makeMethod (methodName, methodPos) = (methodName, (Variable methodName, methodPos))

parseDef :: TEnv -> EveFileLine -> EveExpr
parseDef typeEnv (Def name argData doc typeDecl lines body, pos) = 
    funcall "extend" [convertedBody, (Literal . Record 
        $ [("name", makeString name), ("doc", makeString doc)], pos)] pos
  where
    convertedBody = (Lambda argData True . 
        (maybe id (addTypeCheck . convertTypeDefs) typeDecl) .
        convertBindings lines . convertDefs typeEnv lines $ body, pos)
    addTypeCheck typeDecl body = (TypeCheck (body, typeDecl) body, pos)
    convertTypeDefs :: EveType -> EveType
    convertTypeDefs (TPrim name) = maybe (TPrim name) id $ lookup name typeEnv
    convertTypeDefs (TLiteral datum) = TLiteral datum
    convertTypeDefs (TOr types) = TOr $ map convertTypeDefs types
    convertTypeDefs (TTuple types) = TTuple $ map convertTypeDefs types
    convertTypeDefs (TRecord types) = 
            TRecord $ zip keys $ map convertTypeDefs values
      where (keys, values) = unzip types
    convertTypeDefs (TFunc args ret) = 
        TFunc (map convertTypeDefs args) $ convertTypeDefs ret

parseDef tEnv (Class name superDecl (doc, lines), pos) = 
    funcall "extend" [classBody, (RecordLiteral $ [("proto", (Variable "Function", pos)), 
        ("name", (Literal $ makeString name, pos)), ("doc", (Literal $ makeString doc, pos))], pos)] pos
  where
    classBody = (Funcall (parseDef tEnv defBody) [], pos)
    maybeSuper = maybe id (\var methods -> ("proto", (Variable var, pos)) : methods) superDecl
    defBody = (Def name (ArgExpr [] [] Nothing) doc Nothing lines constr, pos)
    constr = (Lambda (ArgExpr [] [] (Just "args")) True constrBody, pos)
    constrBody = funcall "extend" [funcall "apply" (map (\var -> (Variable var, pos)) ["init", "args"]) pos, proto] pos
    proto = (RecordLiteral [("proto", (RecordLiteral . maybeSuper $ methodRecord lines, pos)), 
                            ("im_receiver", (Literal makeNone, pos))], pos)

readModule :: String -> String -> EveM ModuleDef
readModule moduleName fileText = do
    (imports, typedefs, lines) <- lexer moduleName fileText >>= parseFile 
                 >>= return . parseFileLines
    moduleDefs <- getModules
    importEnv <- mapM loadModule imports >>= return . (baseEnv moduleDefs ++) . concat
    fn <- eval . parseDef typedefs . makeDef $ lines
    Record bindings <- apply (fn { fn_env = importEnv }) []
    return bindings
  where
    modulePos = defaultPos { file = moduleName }
    makeDef lines = (Def ("<module " ++ moduleName ++ ">") (ArgExpr [] [] Nothing) ""
        Nothing lines (RecordLiteral $ methodRecord lines, modulePos), modulePos)
    baseEnv moduleDefs = startingEnv ++ autoImportEnv moduleDefs
    autoImportEnv :: [(String, ModuleDef)] -> Env
    autoImportEnv moduleDefs = concatMap (lookupModule moduleDefs) autoImports
    lookupModule :: [(String, ModuleDef)] -> String -> ModuleDef
    lookupModule moduleDefs name = maybe [] id $ lookup name moduleDefs 

loadModule :: [String] -> EveM ModuleDef
loadModule path = getModules >>= maybeLoad
  where
    moduleName = join "." path
    maybeLoad modules = maybe firstTimeLoad return $ lookup moduleName modules
    firstTimeLoad = liftIO (try $ readModuleText) >>= parseModuleContents
    readModuleText = do
      filename <- return $ "../src/" ++ join "/" path ++ ".eve"
      openFile filename ReadMode >>= hGetContents 
    parseModuleContents (Left err) = 
      throwEveError $ Default $ "Error importing " ++ moduleName ++ ": " ++ show err
    parseModuleContents (Right fileText) = do
      moduleDef <- readModule moduleName fileText
      modify $ addModule moduleDef
      return moduleDef
    addModule moduleDef state = 
        state { interp_modules = (moduleName, moduleDef) : interp_modules state}

evalRepl (Expr expr) = do
    env <- getEnv
    eval expr
evalRepl (ReplImport path) = loadModule path >>= liftM returnValue . mapM addBinding 
  where
    addBinding (var, value) = addTopLevelBinding var value >> return value
    returnValue (first : rest) = first
    returnValue _ = makeNone
evalRepl (Assignment var expr) = do
  value <- eval expr
  addTopLevelBinding var value
  return value

closeOverBindings bindings = result
  where
    result = map editFunctionEnv bindings
    editFunctionEnv (name, (Function argData isShown pos body env fields)) = 
        (name, Function argData isShown pos body (result ++ env) fields)

evalDefaults (ArgExpr args defaults varargs) = do
    values <- mapM eval defaultExprs
    return $ Args args (zip names values) varargs
  where
    (names, defaultExprs) = unzip defaults


eval :: EveExpr -> EveM EveData
eval (Literal val, _) = return val
eval (TupleLiteral args, pos) = withPos pos $ mapM eval args >>= return . makeTuple
eval (RecordLiteral args, pos) = withPos pos $ mapM evalRecord args >>= return . Record
  where evalRecord (label, expr) = 
            do value <- eval expr
               return (label, value)
eval (Variable var, pos) = withPos pos $ do
    env <- getEnv
    maybe (throwEveError $ UnboundVar var) return $ lookup var env
eval (Funcall fnExpr argExpr, pos) = withPos pos $ do
    fn <- eval fnExpr
    args <- mapM eval argExpr
    apply fn args
eval (Cond ((pred, action):rest), pos) = withPos pos $ do
    predResult <- eval pred
    case predResult of
        Bool True _ -> eval action 
        otherwise -> eval (Cond rest, pos)
eval (Lambda argExpr isShown body, pos) = withPos pos $ do
    argData <- evalDefaults argExpr
    env <- getEnv
    return $ makeFunction argData isShown pos body env
eval (Letrec bindings body, pos) = withPos pos $ do
    fns <- mapM evalPair bindings
    env <- getEnv
    pushCall "<letrec>" Nothing False (length fns) (closeOverBindings fns ++ env)
    result <- eval body
    popCall
    return result
  where 
    evalPair (name, body) = do
        result <- eval body
        return (name, result)
eval (TypeCheck (tested, typeDecl) body, pos) = withPos pos $
    eval tested >>= throwIfInvalid typeDecl >> eval body
  where
    throwIfInvalid (TPrim "Int") val@(Int _ _) = return val
    throwIfInvalid (TPrim "Bool") val@(Bool _ _) = return val
    throwIfInvalid (TPrim "Str") val@(String _ _) = return val
    throwIfInvalid (TPrim "Sym") val@(Symbol _ _) = return val
    throwIfInvalid (TLiteral expected) val | val == expected = return val
    -- Function types aren't really checkable until the function is evaluated
    throwIfInvalid (TOr (first : rest)) val = 
        foldl' catchError (throwIfInvalid first val) $ 
            map (const . (flip throwIfInvalid $ val)) rest
    throwIfInvalid (TTuple types) val@(Tuple fields _) = checkAll types fields val
    throwIfInvalid (TRecord types) val@(Record fields) = 
        checkAll (extractVals types) (extractVals $ recordFields fields) val
    throwIfInvalid typeDecl val = throwEveError $ TypeError (show val ++ " is not a " ++ show typeDecl)
    checkAll types fields val = sequence_ (zipWith throwIfInvalid types fields) >> return val
    extractVals = snd . unzip . sortRecord

apply :: EveData -> [EveData] -> EveM EveData
apply (Primitive name fn _) args = do
    env <- getEnv
    pushCall name Nothing True (length args) (zip argLabels args ++ env)
    result <- fn args
    popCall
    return result
  where
    argLabels = take (length args) $ map makeArg (iterate (+ 1) 1)
    makeArg num = "arg" ++ show num

apply (Function argData isShown pos body env fields) args = do
    boundArgs <- bindArgs argData args
    pushCall name (Just pos) isShown (length boundArgs) (boundArgs ++ env)
    result <- eval body
    popCall
    return result
  where
    name = maybe "<function>" show $ lookup "name" fields
    bindArgs (Args argNames defaults varargs) args = case numArgs of
        numArgs | numArgs == numProvided -> return $ bindVarArgs varargs $ boundArgs
        numArgs | numArgs < numProvided && hasVarArgs -> return $ bindVarArgs varargs $ boundArgs
        numArgs | numArgs > numProvided && numArgs - numProvided <= length defaults ->
            return $ bindVarArgs varargs $ defaultsTaken 
                ++ zip (filter (flip notElem $ fst $ unzip defaultsTaken) argNames) args
        numArgs -> throwEveError $ TypeError $ "Wrong number of arguments: expected at least " 
                ++ show (numArgs - length defaults) ++ (if hasVarArgs then "" else ", at most " ++ show numArgs)
                ++ ", found" ++ show args
      where
        boundArgs = zip argNames args
        hasVarArgs = maybe False (const True) varargs
        bindVarArgs Nothing = id
        bindVarArgs (Just varargs) = ((varargs, makeTuple $ drop numArgs args) :)
        numArgs = length argNames
        numProvided = length args
        numDefaults = numArgs - numProvided
        defaultsTaken = take numDefaults defaults
apply val args = throwEveError $ TypeError $ show val ++ " is not a function"

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
applyPrimitive _ = throwEveError $ TypeError $ "Apply takes a function and a sequence"

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
    cmpResult _ = throwEveError $ TypeError "cmp should return an int"

extendPrimitive [dest, source] = return . setAttributes dest $ 
        (attributes source ++ dropAttrs (attrNames source) dest)
  where
    exclude dest source = filter (\(key, val) -> key `notElem` source) dest

attrPrimitive [obj, field@(String name _)] = tryRecord `catchError` tryAttr
  where
    tryRecord = getAttr name obj >>= return . bindReceiver
    tryAttr e | hasAttr "attr" obj = getAttr "attr" obj >>= flip apply [obj, field]
    tryAttr e = throwError e
    bindReceiver = case lookup "im_receiver" $ attributes obj of
        Nothing -> id
        Just (Primitive "None" _ _) -> maybeMakeMethod obj
        Just val -> maybeMakeMethod val
    wrappedFunction receiver fn pos env = Function (Args [] [] (Just "args")) True pos
        (Funcall (Variable "apply", pos) [(Literal fn, pos), (Funcall (Variable "add", pos) 
                [(Literal $ makeTuple [receiver], pos), (Variable "args", pos)], pos)], pos) 
        env [("proto", fn), ("im_self", receiver), ("im_func", fn)]
    maybeMakeMethod rcvr fn@(Primitive _ _ _) = wrappedFunction rcvr fn (Pos "<primitive>" 0 0 0) startingEnv
    maybeMakeMethod _ fn@(Function (Args [] _ Nothing) _ _ _ _ _) = fn
    maybeMakeMethod rcvr fn@(Function _ _ pos _ env _) = wrappedFunction rcvr fn pos env
    maybeMakeMethod _ val = val
attrPrimitive _ = throwEveError $ TypeError "Field access requires an object and a string"

attrRawPrimitive [obj, String field _] = getAttr field obj
attrRawPrimitive _ = throwEveError $ TypeError "Field access requires an object and a string"

convertToTuple [Tuple xs _] = return $ makeTuple xs
convertToTuple [x] = if hasAttr "iter" x 
    then sequenceValues x >>= return . makeTuple 
    else return $ makeTuple [x]
convertToTuple xs = return $ makeTuple xs

convertToRecord [sequence] = sequenceValues sequence >>= mapM toFieldList >>= return . makeRecord
  where
    toFieldList (Tuple [String key _, val] _) = return (key, val)
    toFieldList _ = throwEveError 
        $ TypeError "Records can only be created from a sequence of (String, obj) tuples"

filterRecord fn [dest, fields] = trySequence `catchError` const tryFields
  where
    trySequence = sequenceValues fields >>= mapM extractString >>= modifyFields
    tryFields = modifyFields . attrNames $ fields
    extractString (String val _) = return val
    extractString _ = throwEveError $ TypeError "Second element of restrict must be sequence of strings"
    modifyFields :: [String] -> EveM EveData
    modifyFields source = return . setAttributes dest . 
                    filter (\(key, val) -> fn key source) $ attributes dest
