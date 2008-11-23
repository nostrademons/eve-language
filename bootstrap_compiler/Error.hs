module Error(EveError(..)) where
import SourcePos
import Token
import Control.Monad.Error

data EveError = 
    LexError {
        lexErrorChar :: Char,
        lexErrorPos :: SourcePos
    } 
  | ParseError { parseErrorToken :: Token } 
  | MiscError String deriving (Eq)

instance Show EveError where
    show (LexError c pos) = "Lexical error at " ++ show pos ++ "on character " ++ [c]
    show (ParseError (Token val pos)) =
        "Parse error: unexpected token " ++ show val ++ " at " ++ show pos

instance HasPos EveError where
    pos (LexError _ pos) = pos
    pos (ParseError token) = pos token    

instance Error EveError where
    noMsg = MiscError "Unknown error"
    strMsg str = MiscError str
