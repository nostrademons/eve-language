module Expr(
    Arg(Arg),
    ArgList(ArgList), 
    Expr(Expr), exprVal, exprPos, exprType, 
    ExprValue(..), 
    untypedExpr,
    typedExpr,
    FileLine(FileLine),
    FileLineValue(..),
    DefLine(DefLine),
    DefLineValue(..),   
) where
import SourcePos
import Literal
import Types
import Utils

data Arg = Arg {
    argName :: String,
    argDefault :: Maybe Expr,
    argType :: Maybe Scheme
} deriving (Eq)

instance Show Arg where
    show (Arg name argDefault argType) = name ++ showJust "=" argDefault ++ showJust " as " argType
      where 
        showJust text (Just val) = text ++ show val
        showJust text Nothing = ""

data ArgList = ArgList {
    argListNames :: [Arg],
    argListVarargs :: Maybe String
} deriving (Eq)

instance Show ArgList where
    show (ArgList names varargs) = join ", " $ map show names ++ displayVarargs
      where displayVarargs = maybe [] (\name -> ["*" ++ name]) varargs 

data ExprValue = 
    Literal Literal
  | TupleLiteral [Expr]
  | RecordLiteral [(String, Expr)]
  | Variable String
  | Cond [(Expr, Expr)]
  | Funcall Expr [Expr]
  | Lambda ArgList Expr
  | Letrec [(String, Expr)] Expr
  deriving (Eq)

instance Show ExprValue where
    show (Literal val) = show val
    show (TupleLiteral exprList) = showTuple exprList
    show (RecordLiteral pairList) = showRecord pairList
    show (Variable val) = val
    show (Funcall fn args) = show fn ++ "(" ++ join ", " (map show args) ++ ")"
    show (Cond args) = "Cond: " ++ join ", " (map showClause args)
      where showClause (pred, expr) = show pred ++ "->" ++ show expr
    show (Lambda args body) = "{|" ++ show args ++ "| " ++ show body ++ "}"
    show (Letrec clauses body) = show body ++ " with " ++ 
            join ", " (map showClause clauses)
      where showClause (name, expr) = name ++ " = " ++ show expr

data Expr = Expr {
    exprVal :: ExprValue,
    exprPos :: SourcePos,
    exprType :: Maybe Scheme
} deriving(Eq);

untypedExpr val pos = Expr val pos Nothing
typedExpr (Expr val pos Nothing) typeCheck = Expr val pos $ Just typeCheck
typedExpr (Expr val pos (Just _)) typeCheck =
    error $ "Attempting to add a type " ++ show typeCheck ++ " to typed expression " ++ show val

instance HasPos Expr where pos = exprPos
instance Show Expr where show = show . exprVal

data DefLineValue = 
    Binding {
        def_var :: String,
        def_body :: Expr
    }
  | SequenceUnpack {
        def_vars :: [String],
        def_body :: Expr
    }
  | FinalExpr { defExpr :: Expr }
  | Def {
        def_name :: String,
        def_args :: ArgList,
        def_doc :: String,
        def_type :: Maybe Scheme,
        def_defs :: [DefLine],
        def_body :: Expr
    }
  deriving (Eq)

instance Show DefLineValue where
    show (Binding var expr) = var ++ " = " ++ show expr
    show (SequenceUnpack vars expr) = join ", " vars ++ " = " ++ show expr
    show (FinalExpr expr) = show expr
    show (Def name args doc typeDecl defs body) | length defs == 0 =
        concat ["def ", name, "(", show args, "): ", show body]
    show (Def name args doc typeDecl defs body) =
        concat ["def ", name, "(", show args, "):\n    "] 
            ++ indent (concat $ docString : map show defs) 1 ++ indent (show body) 1
      where docString = if length doc == 0 then "" else doc ++ "\n"

data DefLine = DefLine {
    defVal :: DefLineValue,
    defPos :: SourcePos
} deriving (Eq)

instance Show DefLine where show = show . defVal
instance HasPos DefLine where pos = defPos

data FileLineValue =
    Export [String]
  | Import [String]
  | Definition DefLine
  deriving (Eq)

instance Show FileLineValue where
    show (Export names) = "export " ++ join ", " names
    show (Import path) = "import " ++ join "." path
    show (Definition defLine) = show defLine

data FileLine = FileLine {
    fileLineVal :: FileLineValue,
    fileLinePos :: SourcePos
} deriving(Eq)

instance Show FileLine where show = show . fileLineVal
instance HasPos FileLine where pos = fileLinePos

