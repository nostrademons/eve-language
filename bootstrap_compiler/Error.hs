module Error(EveError(..)) where
import SourcePos
import Token
import Expr
import Types
import Control.Monad.Error

data EveError = 
    LexError {
        lexErrorChar :: Char,
        lexErrorPos :: SourcePos
    } 
  | ParseError { parseErrorToken :: Token } 
  | TypeError {
        typeErrorExpr :: Expr,
        typeErrorErr :: UnificationError
    }
  | MiscError String deriving (Eq)

instance Show EveError where
    show (LexError c pos) = "Lexical error at " ++ show pos ++ "on character " ++ [c]
    show (ParseError (Token val pos)) =
        "Parse error: unexpected token " ++ show val ++ " at " ++ show pos
    show (TypeError (Expr val pos _) err) = concat [show err, " in ", show val, " @ ", show pos]

instance HasPos EveError where
    pos (LexError _ pos) = pos
    pos (ParseError token) = pos token    
    pos (TypeError expr err) = pos expr

instance Error EveError where
    noMsg = MiscError "Unknown error"
    strMsg str = MiscError str
