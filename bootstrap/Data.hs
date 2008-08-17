module Data(EveToken(..), AlexPosn(..), 
            EveExpr(..), EveReplLine(..), EveFileLine(..), 
            EveError(..), EveData(..), EveType(..), Env, 
            ModuleDef, getAccessibleBindings, 
            recordFields, sortRecord, prototype, getAttr, hasAttr,
            EveM, runEveM, getEnv, eveCall, addTopLevelBinding, 
            modules, getStateField, join) where
import Data.List
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)

-- Runtime data

type Env = [(String, EveData)]

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
  | Function [String] EveExpr Env Env

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
attributes (Function _ _ _ fields) = fields

prototype = findPrototype . attributes

lookupAttr missing found name val = maybe (followPrototype $ prototype val) found 
                                        $ lookup name $ attributes val
  where
    followPrototype (Bool False []) = missing
    followPrototype proto = lookupAttr missing found name $ Record $ attributes proto

hasAttr = lookupAttr False $ const True
getAttr name val = lookupAttr (throwError $ MissingField val name) return name val

-- Under current representation, first element of a record is always the prototype
recordFields fields = drop 1 fields

sortRecord = sortBy fieldCompare 
  where fieldCompare (x, _) (y, _) = compare x y
showFields (label, value) = "'" ++ label ++ "': " ++ show value
showTuple val = "[" ++ join ", " (map show val) ++ "]"
showRecord val = "{" ++ join ", " (map showFields $ recordFields val) ++ "}"
showAttributes fields = if recordFields fields == [] then "" else " | " ++ showRecord fields
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
  Function _ body1 _ _ == Function _ body2 _ _ = body1 == body2
  _ == _ = False

instance Show EveData where
  show (Int val fields) = show val ++ showAttributes fields
  show (Bool val fields) = (if val then "True" else "False") ++ showAttributes fields
  show (String val fields) = "'" ++ val ++ "'" ++ showAttributes fields
  show (Symbol val fields) = ":" ++ val ++ showAttributes fields
  show (Tuple val fields) = showTuple val ++ showAttributes fields
  show (SequenceIter val index fields) = "Iterator(" ++ show index ++ ") for " ++ show val 
        ++ showAttributes fields
  show (Record fields) = showRecord fields
  show (RecordIter val index fields) = "Iterator(" ++ show index ++ ") for " ++ show val 
        ++ showAttributes fields
  show (Primitive name _ fields) = name ++ showAttributes fields
  show (Function args body _ fields) = (show $ Lambda args body) ++ showAttributes fields

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

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq)

instance Show AlexPosn where
  show (AlexPn offset line col) = "line " ++ show line ++ ", col " ++ show col

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
  | Binding (Either [String] String) EveExpr
  | TypeDef String EveType
  | Def String [String] String (Maybe EveType) [EveFileLine] EveExpr

instance Show EveFileLine where
  show (Export bindings) = "export " ++ join ", " bindings ++ "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (NakedExpr expr) = show expr
  show (Binding (Left vars) expr) = join ", " vars ++ "=" ++ show expr
  show (Binding (Right var) expr) = var ++ "=" ++ show expr
  show (TypeDef name value) = "typedef " ++ name ++ ": " ++ show value
  show (Def name args docstring Nothing defines body) = 
    "def " ++ name ++ "(" ++ join ", " args ++ "): " ++ show body
  show (Def name args docstring (Just typeExpr) defines body) = 
    "@type(" ++ show typeExpr ++ ")\ndef " ++ name ++ "(" ++ join ", " args ++ "): " ++ show body

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

data EveExpr =
    Literal EveData
  | TupleLiteral [EveExpr]
  | RecordLiteral [(String, EveExpr)]
  | Variable String
  | Cond [(EveExpr, EveExpr)]
  | Funcall EveExpr [EveExpr]
  | Lambda [String] EveExpr
  | Letrec [(String, EveExpr)] EveExpr
  | TypeCheck (EveExpr, EveType) EveExpr
  deriving (Eq)

-- Utility function to generate code fragments for calling other Eve functions
eveCall :: String -> [EveData] -> EveExpr
eveCall name args = Funcall (Variable name) $ map Literal args

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
  show (Lambda args body) = "{|" ++ join ", " args ++ "| " ++ show body ++ "}"
  show (Letrec clauses body) = "Letrec in " ++ show body ++ ":" 
                                ++ join ", " (map showClause clauses)
    where showClause (name, expr) = name ++ " = " ++ show expr
  show (TypeCheck (tested, typeDecl) body) = show tested ++ " as " ++ show typeDecl ++
        (if tested == body then "" else " => " ++ show body)

-- Errors

data EveError =
    LexError Char AlexPosn
  | ParseError EveToken AlexPosn
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

instance Error EveError where
  noMsg = Default "unknown"
  strMsg = Default

-- Interpreter monad
data InterpreterState = Interpreter { 
    env :: Env, 
    modules :: ModuleEnv 
}
type EveM = StateT InterpreterState (ErrorT EveError IO)

getStateField selector = get >>= return . selector

getEnv :: (MonadState InterpreterState m) => m Env
getEnv = getStateField env

addTopLevelBinding :: (MonadState InterpreterState m) => 
                      String -> EveData -> m ()
addTopLevelBinding var value = modify addBinding
  where
    addBinding state = state { env = (var, value) : env state }

runEveM :: EveM a -> Env -> IO (Either EveError (a, InterpreterState))
runEveM monad env = runErrorT $ runStateT monad $ Interpreter env []
