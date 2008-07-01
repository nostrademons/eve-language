module Data(EveToken(..), AlexPosn(..), 
            EveExpr(..), EveReplLine(..), EveFileLine(..), 
            EveError(..), EveData(..), Env, 
            ModuleDef, getAccessibleBindings,
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
  | List [EveData]  -- Eventually will be moved to library
  | Tuple [EveData]
  | Record [(String, EveData)]
  | Primitive String ([EveData] -> EveM EveData)
  | Function [String] EveExpr Env 
  | MultiMethod [EveData]

sortFields = sortBy fieldCompare 
  where fieldCompare (x, _) (y, _) = compare x y
showFields (label, value) = "'" ++ label ++ "': " ++ show value

instance Eq EveData where
  Int x == Int y = x == y
  Bool x == Bool y = x == y
  String x == String y = x == y
  Symbol x == Symbol y = x == y
  List x1 == List x2 = and $ zipWith (==) x1 x2
  Tuple x1 == Tuple x2 = and $ zipWith (==) x1 x2
  Record x1 == Record x2 = and $ zipWith (==) (sortFields x1) (sortFields x2)
  Primitive name1 _ == Primitive name2 _ = name1 == name2
  Function _ body1 _ == Function _ body2 _ = body1 == body2
  MultiMethod m1 == MultiMethod m2 = and $ zipWith (==) m1 m2
  _ == _ = False

instance Show EveData where
  show (Int val) = show val
  show (Bool val) = if val then "True" else "False"
  show (String val) = "'" ++ val ++ "'"
  show (Symbol val) = "Sym(" ++ val ++ ")"
  show (List val) = "List([" ++ join ", " (map show val) ++ "])"
  show (Tuple val) = "[" ++ join ", " (map show val) ++ "]"
  show (Record val) = "{" ++ join ", " (map showFields val) ++ "}"
  show (Primitive name _) = name
  show (Function args body _) = show $ Lambda args body
  show (MultiMethod methods) = show $ methods !! 0

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
    Export [String] String
  | Import [String]
  | Binding String EveExpr
  | Def String [String] [EveFileLine] EveExpr

instance Show EveFileLine where
  show (Export bindings to) = "export " ++ join ", " bindings ++ 
       if length to > 0 then " to " ++ to ++ "\n" else "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (Binding var expr) = var ++ "=" ++ show expr
  show (Def name args defines body) = 
        "def " ++ name ++ "(" ++ join ", " args ++ "): " ++ show body

data EveExpr =
    Literal EveData
  | TupleLiteral [EveExpr]
  | RecordLiteral [(String, EveExpr)]
  | Variable String
  | Cond [(EveExpr, EveExpr)]
  | Funcall EveExpr [EveExpr]
  | Lambda [String] EveExpr
  | Letrec [(String, EveExpr)] EveExpr
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

runEveM monad (env) = runErrorT $ runStateT monad $ Interpreter env []
