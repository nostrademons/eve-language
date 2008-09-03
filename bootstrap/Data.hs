module Data(EveToken(..), SourcePos(..), ArgData(..), ArgExpr(..), args2Vars,
            EveExpr(..), EveReplLine(..), EveFileLine(..), 
            EveError(..), EveStackTrace, EveData(..), EveType(..), TEnv, Env, 
            ModuleDef, getAccessibleBindings, 
            recordFields, sortRecord, showFields, prototype, 
            attributes, setAttributes, getAttr, hasAttr, dropAttrs, attrNames,
            EveM, runEveM, getModules, getEnv, setEnv, addTopLevelBinding, 
            pushCall, popCall, frameVars, throwEveError,
            modules, join) where
import Data.List
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)

-- Runtime data

type Env = [(String, EveData)]

data ArgData = Args [String] [(String, EveData)] (Maybe String) deriving (Eq)

instance Show ArgData where
    show (Args args defaults varargs) = join ", " varArgList
      where
        varArgList = map displayArg args ++ maybe [] (\argName -> ["*" ++ argName]) varargs 
        displayArg arg = maybe arg (\val -> arg ++ "=" ++ show val) $ lookup arg defaults

type LocalVarList = Maybe [String]
args2Vars (ArgExpr args _ varargs) = maybe id (:) varargs $ args

data EveData = 
    Int Int Env
  | Bool Bool Env  
  | String String Env
  | Symbol String Env
  | Tuple [EveData] Env
  | SequenceIter EveData Int Env
  | Record Env
  | RecordIter EveData Int Env
  | Primitive String ([EveData] -> EveM EveData) Env
  | Function {
        fn_args :: ArgData,
        fn_vars :: LocalVarList,
        fn_pos :: SourcePos,
        fn_body :: EveExpr,
        fn_env :: Env,
        fn_fields :: Env
    }

findPrototype :: Env -> EveData
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
setAttributes (Function argData vars pos body env fields) newFields = 
    Function argData vars pos body env newFields

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

recordFields fields = dropAttrFields ["proto"] fields

sortRecord = sortBy fieldCompare 
  where fieldCompare (x, _) (y, _) = compare x y
showFields (label, value) = "'" ++ label ++ "': " ++ show value
showTuple val = "[" ++ join ", " (map show val) ++ "]"
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
  show fn@(Function argData vars pos body _ fields) = maybe (showFunc fn) showMethod $ lookup "im_func" fields
    where 
      showFunc (Function argData _ _ body _ _) = "{| " ++ show argData ++ " | " ++ abbrev (show body) ++ " }"
      showMethod method = "bound method: " ++ showFunc method
      abbrev text = if length text > 40 then take 37 text ++ "..." else text 

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

data SourcePos = Pos String  !Int !Int !Int
	deriving (Eq)

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
        displayArg arg = maybe arg (\val -> arg ++ "=" ++ show val) $ lookup arg defaults

data EveReplLine = 
    Expr EveExpr
  | ReplImport [String]
  | Assignment String EveExpr

instance Show EveReplLine where
  show (Expr expr) = show expr
  show (ReplImport path) = "import " ++ join "." path
  show (Assignment var expr) = var ++ "=" ++ show expr

data EveFileLine =
    Export [String]
  | Import [String]
  | NakedExpr EveExpr
  | Binding (Either [String] String) SourcePos EveExpr
  | TypeDef String EveType
  | Def {
        def_name :: String,
        def_args :: ArgExpr,
        def_doc :: String,
        def_type :: Maybe EveType,
        def_lines :: [EveFileLine],
        def_pos :: SourcePos,
        def_body :: EveExpr
    }
  | Class {
        class_name :: String,
        class_super :: Maybe String,
        class_pos :: SourcePos,
        class_doc_lines :: (String, [EveFileLine])
    }

instance Show EveFileLine where
  show (Export bindings) = "export " ++ join ", " bindings ++ "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (NakedExpr expr) = show expr
  show (Binding (Left vars) pos expr) = join ", " vars ++ "=" ++ show expr
  show (Binding (Right var) pos expr) = var ++ "=" ++ show expr
  show (TypeDef name value) = "typedef " ++ name ++ ": " ++ show value
  show (Def name argData docstring Nothing defines pos body) = 
    "def " ++ name ++ "(" ++ show argData ++ "): " ++ show body
  show (Def name argData docstring (Just typeExpr) defines pos body) = 
    "@type(" ++ show typeExpr ++ ")\ndef " ++ name ++ "(" ++ show argData ++ "): " ++ show body
  show (Class name superclass pos (docstring, lines)) = 
    "class " ++ name ++ maybe "" (\className -> "(" ++ className ++ ")") superclass
            ++ ":\n    " ++ join "\n    " (map show lines)

data EveType =
    TPrim String
  | TLiteral EveData
  | TTuple [EveType]
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

data EveExpr =
    Literal EveData
  | TupleLiteral [EveExpr]
  | RecordLiteral [(String, EveExpr)]
  | Variable String
  | Cond [(EveExpr, EveExpr)]
  | Funcall EveExpr [EveExpr]
  | Lambda ArgExpr LocalVarList SourcePos EveExpr
  | Letrec [(String, EveExpr)] EveExpr
  | TypeCheck (EveExpr, EveType) EveExpr
  deriving (Eq)

join sep [] = ""
join sep ws = foldr1 (\w s -> w ++ sep ++ s) ws

instance Show EveExpr where
  show (Literal val) = show val
  show (TupleLiteral exprList) = "[" ++ join ", " (map show exprList) ++ "]"
  show (RecordLiteral pairList) = "{" ++ join ", " (map showFields pairList) ++ "}"
  show (Variable val) = val
  show (Funcall name args) = show name ++ "(" ++ join ", " (map show args) ++ ")"
  show (Cond args) = "Cond: " ++ join ", " (map showClause args)
    where showClause (pred, expr) = show pred ++ "->" ++ show expr
  show (Lambda argData vars pos body) = "{|" ++ show argData ++ "| " ++ show body ++ "}"
  show (Letrec clauses body) = show body ++ " with " ++ join ", " (map showClause clauses)
    where showClause (name, expr) = name ++ " = " ++ show expr
  show (TypeCheck (tested, typeDecl) body) = show tested ++ " as " ++ show typeDecl ++
        (if tested == body then "" else " => " ++ show body)

-- Stack frames

data StackFrame = Frame {
    frame_name :: String,
    frame_pos :: Maybe SourcePos,
    frame_vars :: [String],
    frame_env :: Env,
    frame_args :: [EveData]
}

instance Show StackFrame where
  show (Frame name pos _ _ args) = name ++ "(" ++ join ", " (map show args) ++ ")" ++ maybe "" showSourcePos pos
    where showSourcePos pos = " at " ++ show pos

-- Errors

data EveError =
    LexError Char SourcePos
  | ParseError EveToken SourcePos
  | UnboundVar String
  | MissingField EveData String
  | TypeError String
  | Default String
  deriving (Eq)

instance Show EveError where
  show (LexError c posn) = "Lexical error at " ++ show posn 
                           ++ " on character " ++ [c]
  show (ParseError tok posn) = "Parse error at " ++ show posn
                               ++ ": unexpected token " ++ show tok
  show (TypeError msg) = "Type error: " ++ msg
  show (UnboundVar var) = "Unbound variable: " ++ var
  show (MissingField record field) = "Missing field: " ++ show record ++ " has no " ++ field
  show (Default str) = "An error occurred: " ++ str

data EveStackTrace = StackTrace [StackFrame] EveError

instance Show EveStackTrace where
  show (StackTrace frames error) = show error ++ ".  Traceback:\n" ++ join "\n" (map (("  " ++) . show) frames)

instance Error EveStackTrace where
  noMsg = StackTrace [] $ Default "unknown"
  strMsg = StackTrace [] . Default

-- Interpreter monad
data InterpreterState = Interpreter { 
    stack :: [StackFrame],
    modules :: ModuleEnv 
}
type EveM = StateT InterpreterState (ErrorT EveStackTrace IO)

getModules :: EveM ModuleEnv
getModules = get >>= return . modules

getEnv :: EveM Env
getEnv = topFrame >>= return . frame_env

setEnv :: Env -> EveM ()
setEnv env = modify $ \s -> s { stack = modifyEnv $ stack s }
  where
    modifyEnv [] = error "top-level frame was popped"
    modifyEnv (top : rest) = top { frame_env = env } : rest

pushCall :: EveData -> [EveData] -> EveM ()
pushCall fn@(Function _ rawVars pos _ env fields) args = 
    addStackFrame $ Frame fName (Just pos) vars env args
  where 
    fName = maybe "<function>" show $ lookup "name" fields
    vars = maybe [] id rawVars
pushCall fn@(Primitive name _ _) args = do
    env <- getEnv
    addStackFrame $ Frame name Nothing [] env args
addStackFrame frame = modify $ \s -> s { stack = frame : stack s }

popCall :: EveM ()
popCall = modify $ \s -> s { stack = tail $ stack s }

topFrame :: EveM StackFrame
topFrame = get >>= return . top . stack
  where
    top [] = error "Top-level stack frame has been popped."
    top (frame : rest) = frame

frameVars :: EveM [String]
frameVars = get >>= return . unpackVars . stack
  where
    unpackVars (Frame _ (Just _) vars _ _ : _) = vars
    -- Needed because locals() introduces its own stack frame
    unpackVars (Frame "locals" _ _ _ _ : rest) = unpackVars rest
    unpackVars _ = []

throwEveError error = do
    frames <- get >>= return . stack
    throwError $ StackTrace frames error

addTopLevelBinding :: (MonadState InterpreterState m) => 
                      String -> EveData -> m ()
addTopLevelBinding var value = modify addBinding
  where
    addBinding state = state { stack = addBindingToBottom $ stack state }
    addBindingToBottom [frame] = [frame { frame_env = (var, value) : frame_env frame }]
    addBindingToBottom (frame : rest) = frame : addBindingToBottom rest

runEveM :: EveM a -> Env -> IO (Either EveStackTrace (a, InterpreterState))
runEveM monad env = runErrorT $ runStateT monad $ Interpreter [Frame "top-level" Nothing [] env []] []
