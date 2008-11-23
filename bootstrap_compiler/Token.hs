module Token(Token(Token), TokenValue(..), extractVar) where
import SourcePos

data TokenValue =
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

instance Show TokenValue where
    show (TokInt val) = show val
    show (TokBool True) = "true"
    show (TokBool False) = "false"
    show (TokString val) = "'" ++ val ++ "'"
    show (TokSym val) = ":" ++ val
    show (TokVar val) = val
    show (TokOp val) = val
    show (TokKeyword val) = val
    show (TokDelim val) = [val]
    show TokNewline = "EOL"
    show TokIndent = "INDENT"
    show TokDedent = "DEDENT"

data Token = Token {
    tokVal :: TokenValue,
    tokPos :: SourcePos 
} deriving (Eq)

instance HasPos Token where
    pos (Token _ p) = p

instance Show Token where
    show (Token t _) = show t

extractVar :: Token -> String
extractVar (Token (TokVar var) _) = var
extractVar _ = error "Extract var called on something other than a TokVar"
