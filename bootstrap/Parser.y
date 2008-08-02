{
module Parser(parseRepl, parseFile, EveData(..), EveExpr(..)) where
import Control.Monad.Error hiding (join)
import Lexer
import Data
}

%name file File
%name replLine ReplLine
%monad { EveM } 
%tokentype { (AlexPosn, EveToken) }

%token
INT   { (_, TokInt $$) }
BOOL  { (_, TokBool $$) }
STR   { (_, TokString $$) }
SYM   { (_, TokSym $$) }
VAR   { (_, TokVar $$) }
EOL   { (_, TokNewline) }
INDENT { (_, TokIndent) }
DEDENT { (_, TokDedent) }
'**'  { (_, TokOp "**") }
'*'   { (_, TokOp "*") }
'/'   { (_, TokOp "/") }
'%'   { (_, TokOp "%") }
'+'   { (_, TokOp "+") }
'-'   { (_, TokKeyword "-") }
'<'   { (_, TokOp "<") }
'>'   { (_, TokOp ">") }
'&'   { (_, TokOp "&") }
'<='  { (_, TokOp "<=") }
'>='  { (_, TokOp ">=") }
'=='  { (_, TokOp "==") }
'!='  { (_, TokOp "!=") }
'='   { (_, TokOp "=")  }
'->'  { (_, TokOp "->") }
'..'  { (_, TokOp "..") }
'and' { (_, TokOp "and") }
'or'  { (_, TokOp "or") }
'not' { (_, TokOp "not") }
'def' { (_, TokKeyword "def") }
'if'  { (_, TokKeyword "if") }
'then'{ (_, TokOp "then") }
'else'{ (_, TokOp "else") }
'cond'{ (_, TokKeyword "cond") }
'as'  { (_, TokOp "as") }
'typedef'   { (_, TokKeyword "typedef") }
','   { (_, TokKeyword ",") }
'.'   { (_, TokOp ".") }
'|'   { (_, TokKeyword "|") }
':'   { (_, TokKeyword ":") }
'@'   { (_, TokKeyword "@") }
'('   { (_, TokDelim '(') }
')'   { (_, TokDelim ')') }
'{'   { (_, TokDelim '{') }
'}'   { (_, TokDelim '}') }
'['   { (_, TokDelim '[') }
']'   { (_, TokDelim ']') }
'import' { (_, TokKeyword "import") }
'export' { (_, TokKeyword "export") }

%left '&'
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

File : FileLine { [$1] }
     | File EOL FileLine { $3 : $1 }

FileLine : SequenceUnpack               { $1 }
         | 'import' DottedIdent         { Import (reverse $2) }
         | 'export' VarList             { Export (reverse $2) }
         | 'typedef' VAR ':' TypeExpr   { TypeDef $2 $4 }
         | DefDecl                      { $1 }

DefDecl     : 'def' VAR '(' ArgList ')' TypeDecl ':' DefBody
    { let (lines, docString, body) = $8 in 
        let (args, newBody) = collectTypeDecls $4 body in 
            Def $2 args docString $6 lines newBody }

DocString   : {- Empty -}       { "" }
            | STR EOL           { $1 }

TypeDecl    : {- Empty -}       { Nothing }
            | 'as' TypeExpr     { Just $2 }

TypeExpr    : VAR                               { TPrim $1 }
            | Literal                           { TLiteral $1 }
            | '[' TypeList ']'                  { TTuple (reverse $2) }
            | '{' LabeledTypeList '}'           { TRecord (reverse $2) }
            | '(' TypeList '->' TypeExpr ')'    { TFunc (reverse $2) $4 }

TypeList    : TypeExpr                  { [$1] }
            | TypeList ',' TypeExpr     { $3 : $1 }

LabeledTypePair     : Label ':' TypeExpr    { ($1, $3) }

LabeledTypeList     : LabeledTypePair                       { [$1] }
                    | LabeledTypeList ',' LabeledTypePair   { $3 : $1 }

DefBody : Expr                              { ([], "", $1) }
        | EOL INDENT DocString DefLineList DEDENT    
                                            { findLastExpr $3 (reverse $4) }

DefLine : Expr                  { NakedExpr $1 }
        | MultiLineExpr         { NakedExpr $1 }
        | SequenceUnpack        { $1 }
        | DefDecl               { $1 }

DefLineList     : DefLine                   { [$1] }
                | DefLineList EOL DefLine { $3 : $1 }

ReplLine : Expr                 { Expr $1 }
         | 'import' DottedIdent { ReplImport (reverse $2) }
         | VAR '=' Expr         { Assignment $1 $3 }

MultiLineExpr   : 'cond' ':' EOL INDENT CondClauseList DEDENT   { Cond (reverse $5) }

SequenceUnpack : VAR '=' Expr          { Binding (Right $1) $3 }
               | VarList '=' Expr      { Binding (Left $1) $3 }

CondClause : Expr ':' Expr      { ($1, $3) }

CondClauseList : CondClause                     { [$1] }
                | CondClauseList EOL CondClause { $3 : $1 }

Expr : Operand             { $1 }
     | '-' Expr %prec NEG  { binop "-" (Literal (Int 0)) $2 }
     | Expr '**' Expr { binop "**" $1 $3 }
     | Expr '*' Expr { binop "*" $1 $3 }
     | Expr '/' Expr { binop "/" $1 $3 }
     | Expr '%' Expr { binop "%" $1 $3 }
     | Expr '+' Expr { binop "+" $1 $3 }
     | Expr '-' Expr { binop "-" $1 $3 }
     | Expr '..' Expr { Funcall (Variable "Range") [$1, $3] }
     | Expr '==' Expr { binop "==" $1 $3 }
     | Expr '!=' Expr { binop "!=" $1 $3 }
     | Expr '>' Expr  { binop ">" $1 $3 }
     | Expr '<' Expr  { binop "<" $1 $3 }
     | Expr '>=' Expr { binop ">=" $1 $3 }
     | Expr '<=' Expr { binop "<=" $1 $3 }
     | Expr '&' Expr    { binop "&" $1 $3 }
     | Expr 'and' Expr  { binop "and" $1 $3 }
     | Expr 'or' Expr   { binop "or" $1 $3 }
     | 'not' Expr       { Funcall (Variable "\\not") [$2] }
     | Expr '->' Expr   { Funcall $3 [$1] }
     | Expr '[' ']'     { Funcall (Variable "get") [$1] }
     | Expr '[' Expr ']' { Funcall (Variable "get") [$3, $1] }
     | 'if' Expr 'then' Expr 'else' Expr
       { Cond [($2, $4), (Literal (Bool True), $6)] }


Operand     : Literal                      { Literal $1 }
            | VAR                          { Variable $1 }
            | '[' ExprList ']'             { TupleLiteral (reverse $2) }
            | '{' LabeledList '}'          { RecordLiteral (reverse $2) }
            | '(' Expr ')'                 { $2 }
            | Expr '(' ExprList ')'        { Funcall $1 (reverse $3) }
            | Expr '(' ')'                 { Funcall $1 [] }
            | '{' '|' ArgList '|' Expr '}' { let (args, newBody) = collectTypeDecls $3 $5 in
                                                Lambda args newBody }
            | Operand 'as' TypeExpr        { TypeCheck ($1, $3) $1 }

Literal     : INT                          { Int $1 }
            | BOOL                         { Bool $1 }
            | STR                          { String $1 }
            | SYM                          { Symbol $1 }
            | '[' ']'                      { Tuple [] }

Label       : STR                      { $1 }
            | VAR                      { $1 }

DottedIdent : VAR { [$1] }
            | DottedIdent '.' VAR      { $3 : $1 }
LabeledPair : Label ':' Expr           { ($1, $3) }

ExprList    : Expr                     { [$1] }
            | ExprList ',' Expr        { $3 : $1 }
VarList     : VAR                      { [$1] }
            | VarList ',' VAR          { $3 : $1 }
ArgList     : {- empty -}              { [] }
            | VAR TypeDecl             { [($1, $2)] }
            | ArgList ',' VAR TypeDecl { ($3, $4) : $1 }
LabeledList : LabeledPair                   { [$1] }
            | LabeledList ',' LabeledPair   { $3 : $1 }

{

parseFile input = file input >>= return . reverse . map replaceFilePartials 
parseRepl input = replLine input >>= return . replaceReplPartials

replaceFilePartials :: EveFileLine -> EveFileLine
replaceFilePartials (Binding var expr) = Binding var $ replacePartials expr
replaceFilePartials line = line

replaceReplPartials :: EveReplLine -> EveReplLine
replaceReplPartials (Expr expr) = Expr $ replacePartials expr
replaceReplPartials (Assignment var expr) = Assignment var $ replacePartials expr
replaceReplPartials line = line

-- Also TODO: Expand the legal positions so that eg. if statements can also have partials
replacePartials :: EveExpr -> EveExpr
replacePartials (Literal val) = Literal val
replacePartials (TupleLiteral args) = maybeLambda TupleLiteral args
replacePartials (RecordLiteral pairs) = maybeLambda reconstruct $ snd $ unzip pairs
  where
    labels = fst $ unzip pairs
    reconstruct = RecordLiteral . zip labels
replacePartials (Variable var) = Variable var
replacePartials (Cond condList) = Cond $ map handleClause condList
  where handleClause (pred, action) = (replacePartials pred, replacePartials action)
replacePartials (Lambda args body) = Lambda args $ replacePartials body
replacePartials (Funcall expr args) = maybeLambda (Funcall (replacePartials expr)) args
replacePartials (TypeCheck (tested, typeDecl) expr) = 
    TypeCheck (replacePartials tested, typeDecl) $ replacePartials expr

maybeLambda exprConstr args =
  if numParams > 0
    then Lambda lambdaList . exprConstr $ substParams lambdaList args
    else exprConstr (map replacePartials args)
  where
    numParams = length . filter (== Variable "?") $ args
    lambdaList = map (("__" ++) . (: [])) $ take numParams ['a'..]
    substParams :: [String] -> [EveExpr] -> [EveExpr]
    substParams (next:params) (Variable "?":args) = Variable next : substParams params args
    substParams params (arg:args) = replacePartials arg : substParams params args
    substParams params [] = []

collectTypeDecls argList body = (reverse $ fst $ unzip argList, newBody)
  where 
    newBody = foldr makeTypeCheck body argList
    makeTypeCheck (varName, Nothing) = id
    makeTypeCheck (varName, Just typeDecl) = TypeCheck (Variable varName, typeDecl)

findLastExpr docString defLines = (lines, docString, last)
  where
    (lines, [NakedExpr last]) = splitAt (length defLines - 1) defLines

binop name left right = Funcall (Variable ('\\':name)) [left, right]

happyError ((posn, token):whatever) = throwError $ ParseError token posn
}
