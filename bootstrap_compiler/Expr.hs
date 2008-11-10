module Expr(
    ArgExpr(ArgExpr), 
    Expr(Expr), 
    untypedExpr,
    typedExpr,
    ExprType(..), 
    FileLine(FileLine)
) where
import SourcePos
import Literal
import Types
import Utils

data ArgExpr = ArgExpr {
    argExprNames :: [String],
    argExprDefaults :: [(String, Expr)],
    argExprVarargs :: Maybe String
} deriving (Eq)

instance Show ArgExpr where
    show (ArgExpr names defaults varargs) = 
        join ", " $ map display names ++ displayVarargs
      where
        displayVarArgs = maybe [] (\name -> ["*" ++ name]) varargs 
        display arg = maybe arg (\val -> arg ++ "=" ++ show val) 
                        $ lookup arg defaults

data ExprType = 
    Literal Literal
  | TupleLiteral [Expr]
  | RecordLiteral [(String, Expr)]
  | Variable String
  | Cond [(Expr, Expr)]
  | Funcall Expr [Expr]
  | Lambda ArgExpr Expr
  | Letrec [(String, Expr)] Expr
  deriving (Eq)

instance Show ExprType where
    show (Literal val) = show val
    show (TupleLiteral exprList) = showTuple exprList
    show (RecordLiteral pairList) = showRecord pairList
    show (Variable val) = val
    show (Funcall name args) = show name ++ "(" ++ join ", " (map show args) ++ ")"
    show (Cond args) = "Cond: " ++ join ", " (map showClause args)
      where showClause (pred, expr) = show pred ++ "->" ++ show expr
    show (Lambda args body) = "{|" ++ show args ++ "| " ++ show body ++ "}"
    show (Letrec clauses body) = show body ++ " with " ++ 
            join ", " (map showClause clauses)
      where showClause (name, expr) = name ++ " = " ++ show expr

data Expr = Expr {
    exprVal :: ExprType,
    exprPos :: SourcePos,
    exprType :: Maybe Type
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
  | TypeDef String Type
  | Def {
        def_name :: String,
        def_args :: ArgExpr,
        def_doc :: String,
        def_type :: Maybe Type,
        def_defs :: [DefLine],
        def_body :: Expr
    }
  deriving (Eq)

instance Show Binding where
    show (Binding var expr) = var ++ " = " ++ show expr
    show (SequenceUnpack vars expr) = join ", " vars ++ " = " ++ show expr
    show (Def name args doc defs body) | length defs == 0 =
        concat ["def ", name, "(", show argData, "): ", body]
    show (Def name args doc defs body) =
        concat ["def ", name, "(", show argData, "):\n    "] 
            ++ indent (concatMap show $ docString : defs) ++ indent (show body)
      where docString = if length doc == 0 then "" else doc ++ "\n"

data DefLine = DefLine {
    defVal :: DefLineValue,
    defPos :: SourcePos
} deriving (Eq)

instance Show DefLine where show = show . defValue
instance HasPos DefLine where pos = defPos

data FileLineValue =
    Export [String]
  | Import [String]
  | Definition DefLine
  deriving (Eq)

instance Show FileLineValue where
    show (Export names) = "export " ++ join ", " bindings
    show (Import path) = "import " ++ join "." path
    show (Definition defLine) = show defLine

data FileLine = FileLine {
    fileLineVal :: FileLineValue,
    fileLinePos :: SourcePos
} deriving(Eq)

instance Show FileLine where show = show . fileLineVal
instance HasPos FileLine where pos = fileLinePos

