module Expr(
    ArgExpr(ArgExpr), 
    Expr(Expr), 
    ExprType(..), 
    FileLine(FileLine)
) where
import SourcePos

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

data Literal = 
    Int { litInt :: Int }
  | Bool { litBool :: Bool }
  | String { litString :: String }
  deriving (Eq)

instance Show Literal where
    show (Int i) = show i
    show (Bool b) = show b
    show (String s) = show s

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

showPair (label, value) = "'" ++ label ++ "': " ++ show value

instance Show ExprValue where
    show (Literal val) = show val
    show (TupleLiteral exprList) = "[" ++ join ", " (map show exprList) ++ "]"
    show (RecordLiteral pairList) = "{" ++ join ", " (map showPair pairList) ++ "}"
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
    exprPos :: SourcePos
}

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
  | Def {
        def_name :: String,
        def_args :: ArgExpr,
        def_doc :: String,
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

