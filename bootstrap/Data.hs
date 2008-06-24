module Data(EveToken(..), AlexPosn(..), 
            EveExpr(..), EveReplLine(..), EveFileLine(..), runIfExpr,
            EveError(..), EveData(..), Env, 
            ModuleDef, getAccessibleBindings,
            EveM, runEveM, getEnv, getTypes, addTopLevelBinding, loadModule,
            TEnv, EveType(..), tBool, tInt, tList, tFunc, tTuple,
            join) where
import Data.List
import Control.Monad.State hiding (join)
import Control.Monad.Error hiding (join)

-- Runtime data

data EveData = 
    Int Integer
  | Bool Bool
  | List [EveData]  -- Eventually will be moved to library
  | Primitive String ([EveData] -> EveData)
  | Function [String] EveExpr Env 

instance Eq EveData where
  Int x == Int y = x == y
  Bool x == Bool y = x == y
  List x1 == List x2 = and $ zipWith (==) x1 x2
  Primitive name1 _ == Primitive name2 _ = name1 == name2
  Function _ body1 _ == Function _ body2 _ = body1 == body2
  _ == _ = False

instance Show EveData where
  show (Int val) = show val
  show (Bool val) = if val then "true" else "false"
  show (List val) = "[" ++ join ", " (map show val) ++ "]"
  show (Primitive name _) = name
  show (Function args body _) = show $ Lambda args body

type Env = [(String, EveData)]

-- Modules

type ModuleBinding = (String, EveData, EveType, String)
type ModuleDef = [ModuleBinding]

getAccessibleBindings :: String -> ModuleDef -> ModuleDef
getAccessibleBindings this = filter (isAccessible this)

isAccessible :: String -> ModuleBinding -> Bool
isAccessible importer (_, _, _, access) = access `isPrefixOf` importer

type ModuleEnv = [(String, ModuleDef)]

-- Tokens

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq)

instance Show AlexPosn where
  show (AlexPn offset line col) = "line " ++ show line ++ ", col " ++ show col

data EveToken =
    TokInt Integer
  | TokBool Bool
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
  show (TokVar val) = val
  show (TokOp val) = val
  show (TokKeyword val) = val
  show (TokDelim val) = show val
  show TokNewline = "EOL"
  show TokIndent = "indent"
  show TokDedent = "dedent"

-- Program fragments

data EveReplLine = 
    Expr EveExpr
  | ReplImport [String]
  | Assignment String EveExpr

instance Show EveReplLine where
  show (Expr expr) = show expr
  show (ReplImport path) = "import " ++ join "." path
  show (Assignment var expr) = var ++ "=" ++ show expr

runIfExpr action (Assignment _ expr) = action expr
runIfExpr action (Expr expr) = action expr
runIfExpr _ _ = return tUnit

data EveFileLine =
    Export [String] String
  | Import [String]
  | Binding String EveExpr

instance Show EveFileLine where
  show (Export bindings to) = "export " ++ join ", " bindings ++ 
       if length to > 0 then " to " ++ to ++ "\n" else "\n"
  show (Import path) = "import " ++ join "." path ++ "\n"
  show (Binding var expr) = var ++ "=" ++ show expr

data EveExpr =
    Literal EveData
  | ListLiteral [EveExpr]
  | Variable String
  | Cond [(EveExpr, EveExpr)]
  | Funcall EveExpr [EveExpr]
  | Lambda [String] EveExpr
  deriving (Eq)

join sep [] = ""
join sep ws = foldr1 (\w s -> w ++ sep ++ s) ws

instance Show EveExpr where
  show (Literal val) = show val
  show (ListLiteral exprList) = "[" ++ join ", " (map show exprList) ++ "]"
  show (Variable val) = val
  show (Funcall name args) = show name ++ "(" ++ join ", " (map show args) ++ ")"
  show (Cond args) = "Cond: " ++ join ", " (map showClause args)
    where showClause (pred, expr) = show pred ++ "->" ++ show expr
  show (Lambda args body) = "{|" ++ join ", " args ++ "| " ++ show body ++ "}"

-- Types

data EveType = 
    TVar String
  | TPrim String
  | TCon String [EveType]
  deriving (Eq)

tBool = TPrim "Bool"
tInt = TPrim "Int"
tUnit = TPrim "Unit"
tList elementType = TCon "List" [elementType]
tTuple = TCon "Tuple"
tFunc argTypes retType = TCon "Function" [tTuple argTypes, retType]

instance Show EveType where
  show (TPrim val) = val
  show (TVar var) = var
  show (TCon "List" [elementType]) = "[" ++ show elementType ++ "]"
  show (TCon "Tuple" types) = "(" ++ join ", " (map show types) ++ ")"
  show (TCon "Function" [from, to]) = show from ++ "->" ++ show to
  show (TCon name args) = name ++ "<" ++ join ", " (map show args) ++ ">"

type TEnv = [(String, EveType)]

-- Errors

data EveError =
    LexError Char AlexPosn
  | ParseError EveToken AlexPosn
  | UnboundVar String
  | TypeCircularity
  | TypeMismatch EveType EveType
  | Default String
  deriving (Eq)

instance Show EveError where
  show (LexError c posn) = "Lexical error at " ++ show posn 
                           ++ " on character " ++ [c]
  show (ParseError tok posn) = "Parse error at " ++ show posn
                               ++ ": unexpected token " ++ show tok
  show (UnboundVar var) = "Unbound variable: " ++ var
  show (TypeCircularity) = "Type error: circular constraints"
  show (TypeMismatch expected found) = "Type error: " ++ show expected ++ 
                                    " did not match " ++ show found
  show (Default str) = "An error occurred: " ++ str

instance Error EveError where
  noMsg = Default "unknown"
  strMsg = Default

-- Interpreter monad
data InterpreterState = Interpreter { 
    env :: Env, 
    types :: TEnv, 
    modules :: ModuleEnv 
}
type EveM = StateT InterpreterState (ErrorT EveError IO)

getStateField selector = get >>= return . selector

getEnv :: (MonadState InterpreterState m) => m Env
getEnv = getStateField env

getTypes :: (MonadState InterpreterState m) => m TEnv
getTypes = getStateField types

loadModule :: (MonadState InterpreterState m) => 
              ([String] -> m ModuleDef) -> [String] -> m ModuleDef
loadModule loader path = getStateField modules >>= maybeLoad
  where
    moduleName = join "." path
    addModule moduleDef state = 
        state { modules = (moduleName, moduleDef) : modules state}
    maybeLoad modules = maybe loadModule return $ lookup moduleName modules
    loadModule = do
      moduleDef <- loader path
      modify $ addModule moduleDef
      return moduleDef

addTopLevelBinding :: (MonadState InterpreterState m) => 
                      String -> EveData -> EveType -> m ()
addTopLevelBinding var value valType = modify addBinding
  where
    addBinding state = state { env = (var, value) : env state,
                               types = (var, valType) : types state }

runEveM monad (env, tEnv) = runErrorT $ runStateT monad $ Interpreter env tEnv []
