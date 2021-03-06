module Data(EveToken(..), defaultPos, SourcePos(..), ArgData(..), ArgExpr(..), 
            EveExpr, EveExprValue(..), EveReplLine(..), EveFileLine, EveFileLineValue(..), 
            EveError(..), EveStackTrace(StackTrace), EveData(..), EveType(..), TEnv, Env, 
            ShowEve(showExpr), ModuleDef, getAccessibleBindings, 
            recordFields, sortRecord, showFields, prototype, 
            attributes, setAttributes, getAttr, hasAttr, dropAttrs, attrNames,
            EveM, runEveM, getModules, addTopLevelBinding, 
            getEnv, setEnv, lookupEnv, makeEnv, extendEnv, unwrapEnv, alterEnv,
            withPos, pushCall, popCall, frameVars, throwEveError, 
            interp_modules, join) where
import Data.List
import Data.IORef
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)

-- Runtime data

type Attributes = [(String, EveData)]

data ArgData = Args [String] [(String, EveData)] (Maybe String) deriving (Eq)

instance Show ArgData where
    show (Args args defaults varargs) = join ", " varArgList
      where
        varArgList = map displayArg args ++ maybe [] (\argName -> ["*" ++ argName]) varargs 
        displayArg arg = maybe arg (\val -> arg ++ "=" ++ show val) $ lookup arg defaults

data EveData = 
    Int Int Attributes
  | Bool Bool Attributes  
  | String String Attributes
  | Symbol String Attributes
  | Tuple [EveData] Attributes
  | SequenceIter EveData Int Attributes
  | Record Attributes
  | RecordIter EveData Int Attributes
  | Primitive {
        fn_name :: String,
        fn_impl :: ([EveData] -> EveM EveData),
        fn_fields :: Attributes
    }
  | Function {
        fn_args :: ArgData,
        fn_is_shown :: Bool,
        fn_pos :: SourcePos,
        fn_body :: EveExpr,
        fn_env :: Env,
        fn_fields :: Attributes
    }

findPrototype :: Attributes -> EveData
findPrototype fields = maybe (Bool False []) id $ lookup "proto" fields

attributes (Int _ fields) = fields
attributes (Bool _ fields) = fields
attributes (String _ fields) = fields
attributes (Symbol _ fields) = fields
attributes (Tuple _ fields) = fields
attributes (SequenceIter _ _ fields) = fields
attributes (Record fields) = fields
attributes (RecordIter _ _ fields) = fields
attributes (Primitive _ _ fields) = fields
attributes (Function _ _ _ _ _ fields) = fields

setAttributes (Int val fields) newFields = Int val newFields
setAttributes (Bool val fields) newFields = Bool val newFields
setAttributes (String val fields) newFields = String val newFields
setAttributes (Symbol val fields) newFields = Symbol val newFields
setAttributes (Tuple val fields) newFields = Tuple val newFields
setAttributes (SequenceIter val index fields) newFields = SequenceIter val index newFields
setAttributes (Record fields) newFields = Record newFields
setAttributes (Primitive name fn fields) newFields = Primitive name fn newFields
setAttributes (Function argData is_shown pos body env fields) newFields = 
    Function argData is_shown pos body env newFields

prototype = findPrototype . attributes

lookupAttr missing found name val = maybe (followPrototype $ prototype val) found 
                                        $ lookup name $ attributes val
  where
    followPrototype (Bool False []) = missing
    followPrototype proto = lookupAttr missing found name $ Record $ attributes proto

hasAttr = lookupAttr False $ const True
getAttr name val = lookupAttr (throwEveError $ MissingField val name) return name val
dropAttrFields names = filter (\(key, val) -> key `notElem` names)
dropAttrs names val = dropAttrFields names $ attributes val
attrNames = fst . unzip . attributes

recordFields fields = dropAttrFields ["proto", "method_receiver"] fields

sortRecord = sortBy fieldCompare 
  where fieldCompare (x, _) (y, _) = compare x y
showFields (label, value) = "'" ++ label ++ "': " ++ showExpr value
showTuple val = "[" ++ join ", " (map showExpr val) ++ "]"
showRecord fields = "{" ++ join ", " (map showFields $ recordFields fields) ++ "}"
eqTuple x1 x2 = and $ zipWith (==) x1 x2
eqRecord x1 x2 = and $ zipWith (==) (sortRecord x1) (sortRecord x2)

instance Eq EveData where
  Int x _ == Int y _ = x == y
  Bool x _ == Bool y _ = x == y
  String x _== String y _ = x == y
  Symbol x _ == Symbol y _ = x == y
  Tuple x1 _ == Tuple x2 _ = eqTuple x1 x2
  SequenceIter x1 i1 _ == SequenceIter x2 i2 _ = i1 == i2 && x1 == x2
  Record x1 == Record x2 = eqRecord x1 x2
  RecordIter x1 i1 _ == RecordIter x2 i2 _ = i1 == i2 && x1 == x2
  Primitive name1 _ _ == Primitive name2 _ _ = name1 == name2
  Function argData1 _ _ body1 _ _ == Function argData2 _ _ body2 _ _ = argData1 == argData2 && body1 == body2
  _ == _ = False

instance Show EveData where
  show (Int val fields) = show val
  show (Bool val fields) = (if val then "True" else "False")
  show (String val fields) = "'" ++ val ++ "'"
  show (Symbol val fields) = ":" ++ val
  show (Tuple val fields) = showTuple val
  show (SequenceIter val index fields) = "Iterator(" ++ show index ++ ") for " ++ show val 
  show (Record fields) = showRecord fields
  show (RecordIter val index fields) = "Iterator(" ++ show index ++ ") for " ++ show val 
  show (Primitive name _ fields) = name
  show fn@(Function argData is_shown pos body _ fields) = 
        maybe (showFunc fn) showMethod $ lookup "im_func" fields
    where 
      showFunc prim@(Primitive {}) = show prim
      showFunc (Function argData _ _ body _ _) = 
            "{| " ++ show argData ++ " | " ++ abbrev (showExpr body) ++ " }"
      showMethod method = "bound method: " ++ showFunc method
      abbrev text = if length text > 40 then take 37 text ++ "..." else text 

-- Environments

-- We can't just use a straight, pure assoclist because Letrec requires that
-- we build the list first and then mutably set its values.
type Env = [(String, IORef EveData)]

makeEnv :: [(String, EveData)] -> IO Env
makeEnv bindings = (mapM newIORef values) >>= return . zip keys
  where (keys, values) = unzip bindings

extendEnv :: [(String, EveData)] -> Env -> EveM Env
extendEnv bindings env = (liftIO $ makeEnv bindings) >>= return . (++ env)

unwrapEnv :: Env -> EveM [(String, EveData)]
unwrapEnv env = mapM extractValue env
  where
    extractValue (var, valueRef) = do
        val <- liftIO $ readIORef valueRef
        return (var, val)

lookupEnv :: String -> EveM EveData
lookupEnv var = do
    env <- getEnv
    maybe (throwEveError $ UnboundVar var) (liftIO . readIORef) $ lookup var env

alterEnv :: String -> EveData -> EveM ()
alterEnv var value = do
    ref <- getEnv >>= maybe (throwEveError $ UnboundVar var) return . lookup var
    liftIO $ writeIORef ref value

-- Modules

type ModuleBinding = (String, EveData)
type ModuleDef = [ModuleBinding]

-- TODO: maybe get rid of these in bootstrap interpreter
getAccessibleBindings :: String -> ModuleDef -> ModuleDef
getAccessibleBindings this = filter (isAccessible this)

isAccessible :: String -> ModuleBinding -> Bool
isAccessible importer (_, _) = True

type ModuleEnv = [(String, ModuleDef)]

-- Tokens

data SourcePos = Pos {
    file :: String,
    offset :: !Int,
    line :: !Int,
    col :: !Int
} deriving (Eq)

defaultPos = Pos "" 0 0 0

instance Show SourcePos where
  show (Pos file offset line col) = file ++ ":" ++ show line ++ ":" ++ show col

data EveToken =
    TokInt Int
  | TokBool Bool
  | TokString String
  | TokSym String
  | TokVar String
  | TokOp String
  | TokKeyword String
  | TokDelim Char
  | TokNewline
  | TokIndent
  | TokDedent
  deriving (Eq)

instance Show EveToken where
  show (TokInt val) = show val
  show (TokBool True) = "true"
  show (TokBool False) = "false"
  show (TokString val) = "'" ++ val ++ "'"
  show (TokSym val) = ":" ++ val
  show (TokVar val) = val
  show (TokOp val) = val
  show (TokKeyword val) = val
  show (TokDelim val) = [val]
  show TokNewline = "EOL"
  show TokIndent = "INDENT"
  show TokDedent = "DEDENT"

-- Program fragments

data ArgExpr = ArgExpr [String] [(String, EveExpr)] (Maybe String) deriving (Eq)

instance Show ArgExpr where
    show (ArgExpr args defaults varargs) = join ", " varArgList
      where
        varArgList = map displayArg args ++ maybe [] (\argName -> ["*" ++ argName]) varargs 
        displayArg arg = maybe arg (\val -> arg ++ "=" ++ showExpr val) $ lookup arg defaults

data EveReplLine = 
    Expr EveExpr
  | ReplImport [String]
  | Assignment String EveExpr

instance Show EveReplLine where
  show (Expr expr) = showExpr expr
  show (ReplImport path) = "import " ++ join "." path
  show (Assignment var expr) = var ++ "=" ++ showExpr expr

type EveFileLine = (EveFileLineValue, SourcePos)
data EveFileLineValue =
    Export [String]
  | Import [String]
  | NakedExpr EveExpr
  | Binding {
        def_var :: Either [String] String,
        def_body :: EveExpr
    }
  | TypeDef String EveType
  | Def {
        def_name :: String,
        def_args :: ArgExpr,
        def_doc :: String,
        def_type :: Maybe EveType,
        def_lines :: [EveFileLine],
        def_body :: EveExpr
    }
  | Class {
        def_name :: String,
        def_super :: Maybe String,
        def_doc_lines :: (String, [EveFileLine])
    }

instance Show EveFileLineValue where
  show (Export bindings) = "export " ++ join ", " bindings ++ "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (NakedExpr expr) = showExpr expr
  show (Binding (Left vars) expr) = join ", " vars ++ "=" ++ showExpr expr
  show (Binding (Right var) expr) = var ++ "=" ++ showExpr expr
  show (TypeDef name value) = "typedef " ++ name ++ ": " ++ show value
  show (Def name argData docstring Nothing defines body) = 
    "def " ++ name ++ "(" ++ show argData ++ "): " ++ showExpr body
  show (Def name argData docstring (Just typeExpr) defines body) = 
    "@type(" ++ show typeExpr ++ ")\ndef " ++ name ++ "(" ++ show argData ++ "): " ++ showExpr body
  show (Class name superclass (docstring, lines)) = 
    "class " ++ name ++ maybe "" (\className -> "(" ++ className ++ ")") superclass

data EveType =
    TPrim String
  | TLiteral EveData
  | TTuple [EveType]
  | TOr [EveType]
  | TRecord [(String, EveType)]
  | TFunc [EveType] EveType

instance Show EveType where
  show (TPrim name) = name
  show (TLiteral datum) = show datum
  show (TTuple fields) = showTuple fields
  show (TRecord fields) = showRecord fields
  show (TFunc args ret) = join ", " (map show args) ++ " -> " ++ show ret

instance Eq EveType where
  TPrim name1 == TPrim name2 = name1 == name2
  TLiteral x1 == TLiteral x2 = x1 == x2
  TTuple x1 == TTuple x2 = eqTuple x1 x2
  TRecord x1 == TRecord x2 = eqRecord x1 x2
  TFunc args1 ret1 == TFunc args2 ret2 = ret1 == ret2 && eqTuple args1 args2

type TEnv = [(String, EveType)]

type EveExpr = (EveExprValue, SourcePos)
data EveExprValue =
    Literal EveData
  | TupleLiteral [EveExpr]
  | RecordLiteral [(String, EveExpr)]
  | Variable String
  | Cond [(EveExpr, EveExpr)]
  | Funcall EveExpr [EveExpr]
  | Lambda ArgExpr Bool EveExpr
  | Letrec [(String, EveExpr)] EveExpr
  | TypeCheck (EveExpr, EveType) EveExpr
  deriving (Eq)

join sep [] = ""
join sep ws = foldr1 (\w s -> w ++ sep ++ s) ws

class ShowEve a where
    showExpr :: a -> String

instance ShowEve EveExpr where
    showExpr (val, pos) = show val

instance ShowEve EveFileLine where
    showExpr (val, pos) = show val

instance ShowEve [EveFileLine] where
    showExpr lines = showTuple lines

instance ShowEve EveReplLine where
    showExpr val = show val

instance ShowEve EveData where
    showExpr val = show val

instance ShowEve EveType where
    showExpr val = show val

instance Show EveExprValue where
    show (Literal val) = show val
    show (TupleLiteral exprList) = "[" ++ join ", " (map showExpr exprList) ++ "]"
    show (RecordLiteral pairList) = "{" ++ join ", " (map showFields pairList) ++ "}"
    show (Variable val) = val
    show (Funcall name args) = showExpr name ++ "(" ++ join ", " (map showExpr args) ++ ")"
    show (Cond args) = "Cond: " ++ join ", " (map showClause args)
      where showClause (pred, expr) = showExpr pred ++ "->" ++ showExpr expr
    show (Lambda argData is_shown body) = "{|" ++ show argData ++ "| " ++ showExpr body ++ "}"
    show (Letrec clauses body) = showExpr body ++ " with " ++ join ", " (map showClause clauses)
      where showClause (name, expr) = name ++ " = " ++ showExpr expr
    show (TypeCheck (tested, typeDecl) body) = showExpr tested ++ " as " ++ show typeDecl ++
        (if tested == body then "" else " => " ++ showExpr body)

-- Stack frames

data StackFrame = Frame {
    frame_name :: String,
    frame_pos :: Maybe SourcePos,
    frame_isShown :: Bool,
    frame_numArgs :: Int,
    frame_env :: Env
}

showStackFrame precedingText (Frame name pos isShown numArgs env) = precedingText ++
    (if isShown 
        then "\n  " ++ name ++ maybe "" showSourcePos pos
        else "")
  where showSourcePos pos = " at " ++ show pos

-- Errors

data EveError =
    LexError Char
  | ParseError EveToken
  | UnboundVar String
  | MissingField EveData String
  | TypeError String
  | Default String
  deriving (Eq)

instance Show EveError where
  show (LexError c) = "Lexical error on character " ++ [c]
  show (ParseError tok) = "Parse error: unexpected token " ++ show tok
  show (TypeError msg) = "Type error: " ++ msg
  show (UnboundVar var) = "Unbound variable: " ++ var
  show (MissingField record field) = "Missing field: " ++ show record ++ " has no " ++ field
  show (Default str) = str

data EveStackTrace = StackTrace SourcePos [StackFrame] EveError

instance Show EveStackTrace where
  show (StackTrace pos frames error) = show error ++ " @ " ++ show pos 
            ++ ".  Traceback:" ++ foldl' showStackFrame "\n  " frames

instance Error EveStackTrace where
  noMsg = strMsg "unknown"
  strMsg = StackTrace defaultPos [] . Default

-- Interpreter monad
data InterpreterState = Interpreter { 
    interp_pos :: SourcePos,
    interp_stack :: [StackFrame],
    interp_modules :: ModuleEnv 
}
type EveM = StateT InterpreterState (ErrorT EveStackTrace IO)

getModules :: EveM ModuleEnv
getModules = get >>= return . interp_modules

getEnv :: EveM Env
getEnv = topFrame >>= return . frame_env

setEnv :: Env -> EveM ()
setEnv env = modify $ \s -> s { interp_stack = modifyEnv $ interp_stack s }
  where
    modifyEnv [] = error "top-level frame was popped"
    modifyEnv (top : rest) = top { frame_env = env } : rest

withPos newPos action = do
    oldPos <- get >>= return . interp_pos
    setPos newPos
    result <- action
    setPos oldPos
    return result
  where
    setPos interp_pos = modify $ \s -> s { interp_pos = newPos }

pushCall :: String -> Maybe SourcePos -> Bool -> Int -> Env -> EveM ()
pushCall name pos isShown args env = 
    addStackFrame $ Frame name pos isShown args env

addStackFrame frame = modify $ \s -> s { interp_stack = frame : interp_stack s }

popCall :: EveM ()
popCall = do
    modify $ \s -> s { interp_stack = tail $ interp_stack s }

topFrame :: EveM StackFrame
topFrame = get >>= return . top . interp_stack
  where
    top [] = error "Top-level interp_stack frame has been popped."
    top (frame : rest) = frame

frameVars :: EveM Attributes
frameVars = get >>= findVars [] . interp_stack
  where
    takeVars :: Attributes -> Int -> Env -> EveM Attributes
    takeVars vars numArgs env = (unwrapEnv $ take numArgs env) >>= return . (vars ++)
    -- Needed because locals() introduces its own interp_stack frame
    findVars vars (Frame "locals" _ _ _ _ : rest) = findVars vars rest
    findVars vars (Frame _ _ False numArgs env : rest) = do
        newVals <- takeVars vars numArgs env 
        findVars newVals rest
    findVars vars (Frame _ _ True numArgs env : _) = takeVars vars numArgs env
    findVars _ _ = return []

throwEveError error = do
    pos <- get >>= return . interp_pos
    frames <- get >>= return . interp_stack
    throwError $ StackTrace pos frames error

addTopLevelBinding :: String -> EveData -> EveM ()
addTopLevelBinding var value = do
    frames <- get >>= return . interp_stack
    newEnv <- extendEnv [(var, value)] . frame_env . last $ frames
    modify $ \s -> s { interp_stack = (init frames ++ [(last frames) { frame_env = newEnv }]) }

runEveM :: EveM a -> Env -> IO (Either EveStackTrace (a, InterpreterState))
runEveM monad env = runErrorT $ runStateT monad $ Interpreter defaultPos [Frame "top-level" Nothing False 0 env] []
