module Token(Token(Tok), TokenType(..), LexError(LexError), ParseError(ParseError)) where
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

data Token = Token {
    tokVal :: TokenType 
    tokPos :: SourcePos 
} deriving (Eq)

instance HasPos Token where
    pos (Token _ p) = p

instance Show Token where
    show (Token t _) = show t

data LexError = LexError {
    lexErrorChar :: Char,
    lexErrorPos :: SourcePos
} deriving (Eq)

instance Show LexError where
    show (LexError c pos) = "Lexical error at " ++ show pos ++ "on character " ++ [c]

data ParseError = ParseError { parseErrorToken :: Token }

instance Show ParseError where
    show (ParseError (Token val pos)) =
        "Parse error: unexpected token " ++ show val ++ " at " ++ show pos
