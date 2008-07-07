module Data(EveToken(..), AlexPosn(..), 
            EveExpr(..), EveReplLine(..), EveFileLine(..), 
            EveError(..), EveData(..), EveType(..), Env, 
            ModuleDef, getAccessibleBindings, sortRecord,
            EveM, runEveM, getEnv, addTopLevelBinding, 
            modules, getStateField, join) where
import Data.List
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)

-- Runtime data

data EveData = 
    Int Int
  | Bool Bool
  | String String
  | Symbol String
  | Tuple [EveData]
  | SequenceIter EveData Int
  | Record [(String, EveData)]
  | RecordIter EveData Int
  | Primitive String ([EveData] -> EveM EveData)
  | Function [String] EveExpr Env 
  | MultiMethod [EveData]

sortRecord = sortBy fieldCompare 
  where fieldCompare (x, _) (y, _) = compare x y
showFields (label, value) = "'" ++ label ++ "': " ++ show value
showTuple val = "[" ++ join ", " (map show val) ++ "]"
showRecord val = "{" ++ join ", " (map showFields val) ++ "}"
eqTuple x1 x2 = and $ zipWith (==) x1 x2
eqRecord x1 x2 = and $ zipWith (==) (sortRecord x1) (sortRecord x2)

instance Eq EveData where
  Int x == Int y = x == y
  Bool x == Bool y = x == y
  String x == String y = x == y
  Symbol x == Symbol y = x == y
  Tuple x1 == Tuple x2 = eqTuple x1 x2
  SequenceIter x1 i1 == SequenceIter x2 i2 = i1 == i2 && x1 == x2
  Record x1 == Record x2 = eqRecord x1 x2
  RecordIter x1 i1 == RecordIter x2 i2 = i1 == i2 && x1 == x2
  Primitive name1 _ == Primitive name2 _ = name1 == name2
  Function _ body1 _ == Function _ body2 _ = body1 == body2
  MultiMethod m1 == MultiMethod m2 = and $ zipWith (==) m1 m2
  _ == _ = False

instance Show EveData where
  show (Int val) = show val
  show (Bool val) = if val then "True" else "False"
  show (String val) = "'" ++ val ++ "'"
  show (Symbol val) = "Sym(" ++ val ++ ")"
  show (Tuple val) = showTuple val
  show (SequenceIter val index) = "Iterator(" ++ show index ++ ") for " ++ show val
  show (Record val) = showRecord val
  show (RecordIter val index) = "Iterator(" ++ show index ++ ") for " ++ show val
  show (Primitive name _) = name
  show (Function args body _) = show $ Lambda args body
  show (MultiMethod methods) = show $ methods 

type Env = [(String, EveData)]

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
  | Binding String EveExpr
  | TypeDef String EveType
  | Def String [String] String (Maybe EveType) [EveFileLine] EveExpr

instance Show EveFileLine where
  show (Export bindings) = "export " ++ join ", " bindings ++ "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (NakedExpr expr) = show expr
  show (Binding var expr) = var ++ "=" ++ show expr
  show (TypeDef name value) = "typedef " ++ name ++ ": " ++ show value
  show (Def name args docstring Nothing defines body) = 
    "def " ++ name ++ "(" ++ join ", " args ++ "): " ++ show body
  show (Def name args docstring (Just typeExpr) defines body) = 
    "@type(" ++ show typeExpr ++ ")\ndef " ++ name ++ "(" ++ join ", " args ++ "): " ++ show body

data EveType =
    TPrim String
  | TTuple [EveType]
  | TRecord [(String, EveType)]
  | TFunc [EveType] EveType

instance Show EveType where
  show (TPrim name) = name
  show (TTuple fields) = showTuple fields
  show (TRecord fields) = showRecord fields
  show (TFunc args ret) = join ", " (map show args) ++ " -> " ++ show ret

instance Eq EveType where
  TPrim name1 == TPrim name2 = name1 == name2
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
  | TypeCheck [(String, EveType)] EveExpr
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
  show (Lambda args body) = "{|" ++ join ", " args ++ "| " ++ show body ++ "}"
  show (Letrec clauses body) = "Letrec in " ++ show body ++ ":" 
                                ++ join ", " (map showClause clauses)
    where showClause (name, expr) = name ++ " = " ++ show expr
  show (TypeCheck _ body) = show body

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
