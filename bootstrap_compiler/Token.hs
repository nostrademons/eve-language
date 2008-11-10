module Token(Token(Tok), TokenType(..)) where
import SourcePos

data TokenType =
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

instance Show TokenType where
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

data Token = Tok {
    tokVal :: TokenType 
    tokPos :: SourcePos 
} deriving (Eq)

instance HasPos Token where
    pos (Tok _ p) = p

instance Show Token where
    show (Tok t _) = show t
