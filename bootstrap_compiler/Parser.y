{
module Parser(parseFile, parseRepl) where
import Control.Monad.Error hiding (join)

import SourcePos
import Token
import Error
import Literal
import Expr
import Types
}

%name eveFile File
%name eveRepl Expr
%monad { Either EveError } 
%tokentype { Token }

%token
INT   { Token (TokInt $$) _ }
BOOL  { Token (TokBool $$) _ }
NONE  { Token (TokKeyword "None") _ }
STR   { Token (TokString $$) _ }
SYM   { Token (TokSym $$) _ }
VAR   { Token (TokVar _) _ }
EOL   { Token (TokNewline) _ }
INDENT { Token (TokIndent) _ }
DEDENT { Token (TokDedent) _ }
'**'  { Token (TokOp "**") _ }
'*'   { Token (TokOp "*") _ }
'/'   { Token (TokOp "/") _ }
'%'   { Token (TokOp "%") _ }
'+'   { Token (TokOp "+") _ }
'-'   { Token (TokKeyword "-") _ }
'<'   { Token (TokOp "<") _ }
'>'   { Token (TokOp ">") _ }
'<='  { Token (TokOp "<=") _ }
'>='  { Token (TokOp ">=") _ }
'=='  { Token (TokOp "==") _ }
'!='  { Token (TokOp "!=") _ }
'='   { Token (TokOp "=") _  }
'->'  { Token (TokOp "->") _ }
'..'  { Token (TokOp "..") _ }
'and' { Token (TokOp "and") _ }
'or'  { Token (TokOp "or") _ }
'not' { Token (TokOp "not") _ }
'def' { Token (TokKeyword "def") _ }
'class'{Token (TokKeyword "class") _ }
'if'  { Token (TokKeyword "if") _ }
'then'{ Token (TokOp "then") _ }
'else'{ Token (TokOp "else") _ }
'cond'{ Token (TokKeyword "cond") _ }
'as'  { Token (TokOp "as") _ }
'where'{ Token (TokOp "where") _ }
'typedef'   { Token (TokKeyword "typedef") _ }
','   { Token (TokKeyword ",") _ }
'.'   { Token (TokOp ".") _ }
'|'   { Token (TokOp "|") _ }
'&'   { Token (TokOp "&") _ }
'~'   { Token (TokOp "~") _ }
':'   { Token (TokKeyword ":") _ }
'@'   { Token (TokKeyword "@") _ }
'('   { Token (TokDelim '(') _ }
')'   { Token (TokDelim ')') _ }
'{'   { Token (TokDelim '{') _ }
'}'   { Token (TokDelim '}') _ }
'['   { Token (TokDelim '[') _ }
']'   { Token (TokDelim ']') _ }
'import' { Token (TokKeyword "import") _ }
'export' { Token (TokKeyword "export") _ }

%left '&' '~'
%left '|'
%left 'and' 'or'
%nonassoc 'not'
%nonassoc '!=' '==' '>=' '<=' '>' '<'
%nonassoc '..'
%left '+' '-'
%left '*' '/' '%'
%left '**'
%left NEG
%left '->'

%%

File 
  : FileLine 
    { [$1] }
  | File EOL FileLine 
    { $3 : $1 }

FileLine 
  : SequenceUnpack               
    { FileLine (Definition $1) $ pos $1 }
  | 'import' DottedIdent         
    { FileLine (Import $ reverse $2) $ pos $1 }
  | 'export' VarList             
    { FileLine (Export $ reverse $2) $ pos $1 }
  | DefDecl                      
    { FileLine (Definition $1) $ pos $1 }

DefDecl     
  : 'def' VAR '(' VarArgList ')' TypeDecl ':' DefBody
    { let (doc, lines, body) = $8 in 
        DefLine (Def (extractVar $2) $4 doc $6 lines body) $ pos $1 }

DocString   
  : {- Empty -}       
    { "" }
  | STR EOL           
    { $1 }

DefBody 
  : Expr                                      
    { ("", [], $1) }
  | EOL INDENT DocString DefLineList DefExpr DEDENT   
    { ($3, reverse $ tail $4, $5) }

DefLine 
  : SequenceUnpack EOL        
    { $1 }
  | DefDecl EOL               
    { $1 }

DefExpr 
  : Expr                  
    { $1 }
  | MultiLineExpr         
    { $1 }

DefLineList     
  : {- empty -}           
    { [] }
  | DefLineList DefLine   
    { $2 : $1 }

MultiLineExpr   
  : 'cond' ':' EOL INDENT CondClauseList DEDENT   
    { untypedExpr (Cond $ reverse $5) $ pos $1 }

SequenceUnpack 
  : VAR '=' Expr          
    { DefLine (Binding (extractVar $1) $3) $ pos $2 }
  | VarList '=' Expr      
    { DefLine (SequenceUnpack (reverse $1) $3) $ pos $2 }

CondClause 
  : Expr ':' Expr      
    { ($1, $3) }

CondClauseList 
  : CondClause                     
    { [$1] }
  | CondClauseList EOL CondClause 
    { $3 : $1 }

Expr 
  : Operand             
    { $1 }
  | '-' Expr %prec NEG  
    { funcall (pos $1) "sub" [untypedExpr (Literal $ LitInt 0) $ pos $1, $2] }
  | Expr '**' Expr 
    { funcall (pos $2) "pow" [$1, $3] }
  | Expr '*' Expr 
    { funcall (pos $2) "mul" [$1, $3] }
  | Expr '/' Expr 
    { funcall (pos $2) "div" [$1, $3] }
  | Expr '%' Expr 
    { funcall (pos $2) "mod" [$1, $3] }
  | Expr '+' Expr 
    { funcall (pos $2) "add" [$1, $3] }
  | Expr '-' Expr 
    { funcall (pos $2) "sub" [$1, $3] }
  | Expr '..' Expr 
    { funcall (pos $2) "Range" [$1, $3] }
  | Expr '==' Expr 
    { funcall (pos $2) "eq" [$1, $3] }
  | Expr '!=' Expr 
    { funcall (pos $2) "ne" [$1, $3] }
  | Expr '>' Expr  
    { funcall (pos $2) "gt" [$1, $3] }
  | Expr '<' Expr  
    { funcall (pos $2) "lt" [$1, $3] }
  | Expr '>=' Expr 
    { funcall (pos $2) "ge" [$1, $3] }
  | Expr '<=' Expr 
    { funcall (pos $2) "le" [$1, $3] }
  | Expr 'and' Expr  
    { funcall (pos $2) "and_" [$1, $3] }
  | Expr 'or' Expr   
    { funcall (pos $2) "or_" [$1, $3] }
  | 'not' Expr       
    { funcall (pos $1) "not_" [$2] }
  | Expr '|' Expr    
    { funcall (pos $2) "extend" [$1, $3] }
  | Expr '&' Expr    
    { funcall (pos $2) "restrict" [$1, $3] }
  | Expr '~' Expr    
    { funcall (pos $2) "exclude" [$1, $3] }
  | Expr '->' Expr   
    { untypedExpr (Funcall $3 [$1]) $ pos $2 }
  | 'if' Expr 'then' Expr 'else' Expr
    { untypedExpr (Cond [($2, $4), 
      (untypedExpr (Literal $ LitBool True) $ pos $1, $6)]) $ pos $1 }

{- Faking the position on Literal so we don't need to plumb the position back 
    up from them; errors never happen on literals anyway. -}
Operand     
  : Literal                      
    { untypedExpr (Literal $1) defaultPos }
  | VAR                          
    { untypedExpr (Variable $ extractVar $1) $ pos $1 }
{-
  | '(' ')'                      
    { untypedExpr (TupleLiteral []) $ pos $1 }
-}
  | '(' Expr ',' ')'             
    { untypedExpr (TupleLiteral $ [$2]) $ pos $1 }
  | '(' Expr ',' ExprList ')'    
    { untypedExpr (TupleLiteral $ $2 : reverse $4) $ pos $1 }
  | '{' LabeledList '}'          
    { untypedExpr (RecordLiteral (reverse $2)) $ pos $1 }
  | '(' Expr ')'                 
    { $2 }
  | Operand '[' ']'                   
    { funcall (pos $2) "get" [$1] }
  | Operand '[' Expr ']'              
    { funcall (pos $2) "get" [$1, $3] }
  | Operand '[' Expr ':' Expr ']'     
    { funcall (pos $2) "slice" [$1, $3, $5] }
  | Operand '(' ExprList ')'          
    { untypedExpr (Funcall $1 (reverse $3)) $ pos $2 }
  | Operand '(' ')'                   
    { untypedExpr (Funcall $1 []) $ pos $2 }
  | Operand '(' '*' Operand ')'       
    { funcall (pos $2) "apply" [$1, $4] }
  | Operand '(' ExprList ',' '*' Operand ')' 
    { funcall (pos $2) "apply" [$1, funcall (pos $2) "add" 
        [untypedExpr (TupleLiteral (reverse $3)) $ pos $2, $6]] }
  | Operand '.' VAR                 
    { funcall (pos $2) "attr" 
        [$1, untypedExpr (Literal . LitString . extractVar $ $3) $ pos $2] }
  | '{' '|' VarArgList '|' Expr '}' 
    { untypedExpr (Lambda $3 $5) $ pos $1 }
  | Operand 'as' TypeScheme        
    { typedExpr $1 $3 }

TypeDecl    
  : {- Empty -}       
    { Nothing }
  | 'as' TypeScheme     
    { Just $2 }

TypeScheme  
  : TypeExpr                              
    { Scheme [] $1 }
  | TypeExpr 'where' TypeConstraintList   
    { Scheme (reverse $3) $1 }

TypeConstraint  
  : VAR '(' VAR ')'       
    { IsIn (extractVar $1) (TVar $ Tyvar (extractVar $3) 0) }

TypeConstraintList
  : TypeConstraint
    { [$1] }
  | TypeConstraintList ',' TypeConstraint
    { $3 : $1 }

TypeExpr    
  : VAR                       
    { TCon $ Tycon (extractVar $1) 0 }
  | VAR '<' TypeList '>'              
    { TAp (TCon $ Tycon (extractVar $1) (length $3)) $ (reverse $3) }
  | TypeTuple                 
    { tTuple $1 }
  | TypeTuple '->' TypeExpr   
    { tFunc $1 $3 }
  | TypeExpr '->' TypeExpr    
    { tFunc [$1] $3 }

TypeVar     
  : VAR       
    { $1 }

TypeList        
  : TypeExpr                  
    { [$1] }
  | TypeList ',' TypeExpr     
    { $3 : $1 }

TypeTuple   
  : '(' ')'                       
    { [] }
  | '(' TypeExpr ',' ')'          
    { [$2] }
  | '(' TypeExpr ',' TypeList ')'         
    { $2 : reverse $4 }

TypeAlternatives    
  : TypeExpr                          
    { [$1] }
  | TypeAlternatives 'or' TypeExpr    
    { $3 : $1 }

LabeledTypePair     
  : Label ':' TypeExpr    
    { ($1, $3) }

LabeledTypeList     
  : LabeledTypePair                       
    { [$1] }
  | LabeledTypeList ',' LabeledTypePair   
    { $3 : $1 }


Literal     
  : INT                          
    { LitInt $1 }
  | BOOL                         
    { LitBool $1 }
  | STR                          
    { LitString $1 }

Label       
  : STR                      
    { $1 }
  | VAR                      
    { extractVar $1 }

DottedIdent 
  : VAR 
    { [extractVar $1] }
  | DottedIdent '.' VAR      
    { extractVar $3 : $1 }

LabeledPair 
  : Label ':' Expr           
    { ($1, $3) }

VarList     
  : VAR                      
    { [extractVar $1] }
  | VarList ',' VAR          
    { extractVar $3 : $1 }

DefaultDecl 
  : {- Empty -}           
    { Nothing }
  | '=' Operand           
    { Just $2 }

ArgList     
  : {- empty -}                          
    { [] }
  | VAR DefaultDecl TypeDecl             
    { [Arg (extractVar $1) $2 $3] }
  | ArgList ',' VAR DefaultDecl TypeDecl 
    { (Arg (extractVar $3) $4 $5) : $1 }

VarArgList  
  : ArgList                  
    { ArgList $1 Nothing }
  | '*' VAR                  
    { ArgList [] $ Just . extractVar $ $2 }
  | ArgList ',' '*' VAR      
    { ArgList $1 $ Just . extractVar $ $4 }

ExprList        
  : Expr                     
    { [$1] }
  | ExprList ',' Expr        
    { $3 : $1 }

LabeledList     
  : LabeledPair                   
    { [$1] }
  | LabeledList ',' LabeledPair   
    { $3 : $1 }

{

parseFile :: [Token] -> Either EveError [FileLine]
parseFile input = eveFile input >>= return . reverse 

parseRepl :: [Token] -> Either EveError Expr
parseRepl input = eveRepl input

funcall pos name args = untypedExpr (Funcall (untypedExpr (Variable name) pos) args) pos

happyError (token:whatever) = Left $ ParseError token 
}
